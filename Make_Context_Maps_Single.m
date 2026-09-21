% Using cmocean and m_map functions
addpath('/Users/yannik/Desktop/Blue_Shark_Project/Paper/Code/')
addpath('/Users/yannik/Desktop/Blue_Shark_Project/Paper/Code/packages_needed_matlab/cmocean/')
addpath('/Users/yannik/Desktop/Blue_Shark_Project/Paper/Code/packages_needed_matlab/m_map/')
addpath('/Users/yannik/Desktop/Blue_Shark_Project/Paper/Code/packages_needed_matlab/')

% Graphical preferences to make fonts of axes and labels nice and readable
set(0,'defaultaxesfontsize',14)
set(0,'defaulttextfontsize',14)
set(0,'DefaultFigureColor','w')
set(0,'defaulttextfontname','helvetica')

% Define maps: extent and projection
lonv = [174 186]; % pick longitude range
latv = [-36 -26]; % pick latitude range
m_proj('lambert','long',lonv,'lat',latv);

% Defining beginning and end of animation in format yyyy mm dd
day0=[2013 12 23]; % start of time series
dayf=[2013 12 31]; % end of time series
time_lag = 0;
numdays=datenum(dayf)-datenum(day0)+1;

%% Formating for overlaying tracks of individual sharks
bsh_pred=readtable('/Users/yannik/Desktop/Blue_Shark_Project/Paper/Data/all_bsh_pred-collocated.csv');
bsh_pred_dtm= datenum(bsh_pred.date);

bsh_pred_126617 = regexp(bsh_pred.id,'160424_2013_134170.*'); % Specify shark id

for qq=1:size(bsh_pred_126617,1)
bsh_pred_126617_tmp(qq) = ~isempty(bsh_pred_126617{qq});
end

bsh_pred_126617_idx = find(bsh_pred_126617_tmp);

%% Main loop that goes through all dates have data for and make plots to be compiled into animation
for qq = 1:numdays
   
 % Calculate the date
 dayref = datevec(datenum(day0)+qq);
  
 % Find locations corresponding to the day of reference with time lag
 yrr_bsh = find(bsh_pred_dtm(bsh_pred_126617_idx) >= datenum(dayref) - time_lag & bsh_pred_dtm(bsh_pred_126617_idx) <= datenum(dayref));

 %% SLA plotting
 % Define format of SLA files
 folder_sla = '/Users/yannik/Desktop/Blue_Shark_Project/Paper/Data/Ocean_Currents';
 filename_sla = sprintf('%s/dt_global_allsat_phy_l4_%02d%02d%02d_20190101.nc',folder_sla,dayref(1),dayref(2),dayref(3));
 [lon_sla,lat_sla,sla] = read_SLA_dt(filename_sla,lonv,latv); 

 %% SST plotting
 % Define format of SST file
 folder_sst = '/Users/yannik/Desktop/Blue_Shark_Project/Paper/Data/SST_CCI';
 filename_sst = sprintf('%s/%02d%02d%02d120000-ESACCI-L4_GHRSST-SSTdepth-OSTIA-GLOB_CDR2.0-v02.0-fv01.0.nc',folder_sst,dayref(1),dayref(2),dayref(3));
 [lon_sst,lat_sst,sst]= read_SST_dt(filename_sst,lonv,latv);  % and read it!

 % Plot SST and SLA contures
 figure(1),clf,m_pcolor(lon_sst,lat_sst,sst-273.15); % then we plot it
 [long_sla,latg_sla]= meshgrid(lon_sla,lat_sla);
 [long_sst,latg_sst] = meshgrid(lon_sst,lat_sst);
 sla_filtered = sla - mean(sla(:),'omitnan');
 sla_interp = interp2(long_sla,latg_sla,sla_filtered,long_sst,latg_sst);
 % Colour code SLA contour lines (either + = white or - = black) 
 hold on,m_contour(lon_sst,lat_sst,sla_interp,[0:0.01:1],'w') % Positive SLA contours showing ACEs
 hold on,m_contour(lon_sst,lat_sst,sla_interp,[-1:0.01:0],'k') % Negative SLA contours showing CEs
 % SST colourbar
 caxis([18 28]); % change colourbar extremes
 colorbar % add the colorbar
 cmocean('thermal') % cool thermal cmocean colorbar

 title(sprintf('%02d-%02d-%-2d',dayref(1),dayref(2),dayref(3))) % set title

 %% Overlay track on map
 % Track overlayed for each day with defined time lag
 hold on, m_plot(bsh_pred.lon(bsh_pred_126617_idx(yrr_bsh)),bsh_pred.lat(bsh_pred_126617_idx(yrr_bsh)),'Marker','o','markersize',18)
 m_plot(bsh_pred.lon(bsh_pred_126617_idx(yrr_bsh)),bsh_pred.lat(bsh_pred_126617_idx(yrr_bsh)),'k.','Marker','o','markersize',18,'markerfacecolor', 'r')

 % m_gshhs_c('patch',[0 0 0])
 m_gshhs_f('patch',[0 0 0]) % more detailed coastlines
 m_grid
 set(gcf,'PaperPosition',[1 1 25 15])
 print(figure(1),'-dpng',sprintf('%02d%02d%02d.png',dayref(1),dayref(2),dayref(3)))

end