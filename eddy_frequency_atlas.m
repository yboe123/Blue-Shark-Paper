% Set working directories
addpath('/Users/yannik/Desktop/Blue_Shark_Project/Paper/Code/')
addpath('/Users/yannik/Desktop/Blue_Shark_Project/Paper/Code/packages_needed_matlab/cmocean/')
addpath('/Users/yannik/Desktop/Blue_Shark_Project/Paper/Code/packages_needed_matlab/m_map/')
addpath('/Users/yannik/Desktop/Blue_Shark_Project/Paper/Code/packages_needed_matlab/')
addpath('/Users/yannik/Desktop/Blue_Shark_Project/Paper/Data/eddies/')
load('/Users/yannik/Desktop/Blue_Shark_Project/Paper/Code/Eddie Scripts/eddy_frequency.mat')


%% Load and formate shark data for overlaying tracks
data=readtable('/Users/yannik/Desktop/Blue_Shark_Project/Paper/Data/bsh_pred_collocated_with_states.csv');


bsh_pred = data(data.bathy <= -800, :); % apply filter, bathymetry <= 800m

idx1 = bsh_pred.state == 2; % Resident
idx2 = bsh_pred.state == 1; % Travelling

%% Load eddy data from CSV files
eddy_anti = readtable('eddies_Anticyclonic_3.2_DT_20250315_bathy.csv'); % for ACEs
eddy_cyclo = readtable('eddies_Cyclonic_3.2_DT_20250315_bathy.csv'); % for CEs

%% Combine both eddy datasets
eddy_all = [eddy_anti; eddy_cyclo];

% Adjust variable names
eddies.x = eddy_all.lon;
eddies.y = eddy_all.lat;
eddies.mtime = datenum(eddy_all.dt);
eddies.Ls = eddy_all.rad;

%% Filter for eddy type
% % For ACE only
% eddies.x = eddy_anti.lon;
% eddies.y = eddy_anti.lat;
% eddies.mtime = datenum(eddy_anti.dt);
% eddies.Ls = eddy_anti.rad;
 
% % For CE only
% eddies.x = eddy_cyclo.lon;
% eddies.y = eddy_cyclo.lat;
% eddies.mtime = datenum(eddy_cyclo.dt);
% eddies.Ls = eddy_cyclo.rad;

%% Define maps: extent and projection
% lonvv = 162:0.25:210;
% latvv = -40:0.25:-10;
% m_proj('lambert','long',[162 210],'lat',[-45 -10]);

%% Define maps: extent and projection
lonv = [162 208]; % pick longitude range
latv = [-40 -10]; % pick latitude range
m_proj('lambert','long',lonv,'lat',latv);

% Create grid midpoints for mapping purposes
lonc = lonvv(1:end-1) + diff(lonvv)/2;
latc = latvv(1:end-1) + diff(latvv)/2;

%% Set date limits
start_date = datenum('2013-03-11');
end_date = datenum('2015-04-13');
tt = find(eddies.mtime >= start_date & eddies.mtime <= end_date);

%% Calculate Probability
% Preallocate the output matrix with correct dimensions
isto = zeros(length(lonc), length(latc));

for jj = 1:length(lonc)
    for kk = 1:length(latc)
        disty = sqrt((eddies.x(tt) - lonc(jj)).^2 + (eddies.y(tt) - latc(kk)).^2);
        idx = find(disty <= eddies.Ls(tt)./110);  % 1 deg ≈ 110 km
        isto(jj,kk) = length(unique(eddies.mtime(tt(idx))));
    end
end
% Use the time span of your eddy data if no shark data is used
prbab = isto ./ (max(eddies.mtime(tt)) - min(eddies.mtime(tt))) * 100;

%% Plot probability map
figure(2),clf,
m_pcolor(lonc,latc,prbab') % transpose for correct orientation
shading interp
colorbar
caxis([0 60]) %change colourbar extremes


%% Overlay Sharks Tracks
% All tracks
%hold on,m_plot(bsh_pred.lon,bsh_pred.lat,'k.','markersize',1)
%m_plot(bsh_pred.lon,bsh_pred.lat,'k.','Marker','o','markersize',3,'markerfacecolor','k')

% Plot shark locations collocated - traveling behavior
hold on
m_plot(bsh_pred.lon(idx2),bsh_pred.lat(idx2),'.','markersize',1)
trav = m_plot(bsh_pred.lon(idx2),bsh_pred.lat(idx2),'.', 'Color', [0.25 0.25 0.25], 'markersize',3,'markerfacecolor', [0.25 0.25 0.25],'marker','o')

% Plot shark locations - resident behavior
m_plot(bsh_pred.lon(idx1),bsh_pred.lat(idx1),'.','markersize',1)
res = m_plot(bsh_pred.lon(idx1),bsh_pred.lat(idx1),'.', 'Color', 'm','markersize',3,'markerfacecolor','m','marker','o')


%% Format figure
% m_gshhs_c('patch',[1 1 1]) % less detailed coastlines (load faster)
m_gshhs_f('patch',[1 1 1]) % more detailed coastlines
m_grid
