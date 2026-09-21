% Using cmocean and m_map functions
addpath('/Users/yannik/Desktop/Blue_Shark_Project/Paper/Code/')
addpath('/Users/yannik/Desktop/Blue_Shark_Project/Paper/Code/packages_needed_matlab/cmocean/')
addpath('/Users/yannik/Desktop/Blue_Shark_Project/Paper/Code/packages_needed_matlab/m_map/')
addpath('/Users/yannik/Desktop/Blue_Shark_Project/Paper/Code/packages_needed_matlab/')

%% Graphical preferences to make fonts of axes and labels nice and readable
set(0,'defaultaxesfontsize',14)
set(0,'defaulttextfontsize',14)
set(0,'DefaultFigureColor','w')
set(0,'defaulttextfontname','helvetica')

%% Define maps: extent and projection
lonv = [162 208]; % pick longitude range
latv = [-40 -10]; % pick latitude range
m_proj('lambert','long',lonv,'lat',latv);

%% Formating data for overlaying tracks
data=readtable('/Users/yannik/Desktop/Blue_Shark_Project/Paper/Data/all_bsh_pred-collocated.csv');

bsh_pred = data(data.bathy <= -800, :);
bsh_pred_filter = data(data.bathy <= -800 & data.tbin <= 0.5, :);

% Split tracks into ACE and CE cores
idx1 = bsh_pred_filter.etype == 1; % ACE cores
idx2 = bsh_pred_filter.etype == -1; % CE cores
idx3 = bsh_pred.etype == 1; % ACE
idx4 = bsh_pred.etype == -1; % CE


%% Overlay Shark Tracks
% Plot all Sharks Tracks
hold on,m_plot(bsh_pred.lon,bsh_pred.lat,'w.','markersize',1)
m_plot(bsh_pred.lon,bsh_pred.lat, 'k.', 'markersize',2,'markerfacecolor','0.5,0.5,0.5','marker','o')

% Plot shark locations collocated in ACE (all)
hold on,m_plot(bsh_pred.lon(idx3),bsh_pred.lat(idx3),'w.','markersize',1)
m_plot(bsh_pred.lon(idx3),bsh_pred.lat(idx3),'rs','markersize',2,'markerfacecolor','r','marker','o')

% Plot shark locations collocated in ACE (cores only)
hold on,m_plot(bsh_pred_filter.lon(idx1),bsh_pred_filter.lat(idx1),'k.','markersize',1)
m_plot(bsh_pred_filter.lon(idx1),bsh_pred_filter.lat(idx1),'k.','markersize',5,'markerfacecolor','r','marker','o')

% Plot shark locations collocated in CE (all)
hold on,m_plot(bsh_pred.lon(idx4),bsh_pred.lat(idx4),'w.','markersize',1)
m_plot(bsh_pred.lon(idx4),bsh_pred.lat(idx4),'bs','markersize',2,'markerfacecolor','b','marker','o')

% Plot shark locations collocated in CE (cores only)
hold on,m_plot(bsh_pred_filter.lon(idx2),bsh_pred_filter.lat(idx2),'k.','markersize',1)
m_plot(bsh_pred_filter.lon(idx2),bsh_pred_filter.lat(idx2),'k.','markersize',5,'markerfacecolor','b','marker','o')

%% Print and format figure
m_gshhs_f('patch',[0 0 0])
m_grid
set(gcf,'PaperPosition',[1 1 25 15])
print(figure(1),'-dpng',sprintf('Figures/All_Tracks.png'))