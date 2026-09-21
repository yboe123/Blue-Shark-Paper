%% Defining data
data = readtable('/Users/yannik/Desktop/Blue_Shark_Project/Paper/Data/bsh_pred-collocated_states_filtered.csv');

tbl = data(data.bathy <= -800, :);

%% Eddy centric data visualisation
idx1 = tbl.etype == 1;   % ACE +
idx2 = tbl.etype == -1;  % CE + 
idx3 = tbl.etype == 1 & tbl.State == 1;
idx4 = tbl.State == 2;

figure;
hold on

%% 2D Histogram (Anti-cyclonic eddies)
histogram2(tbl.dx(idx1), tbl.dy(idx1),...
    'XBinEdges', -2.25:0.25:2.25,...
    'YBinEdges', -2.25:0.25:2.25,...
    'DisplayStyle', 'tile',...
    'EdgeColor', 'none',...
    'ShowEmptyBins', 'off');

colormap(parula)
% colorbar
clim([1 20]) % adjust as needed

%% 2D Histogram (Cylonic eddies)
% histogram2(tbl.dx(idx2), tbl.dy(idx2),...
%     'XBinEdges', -2:0.25:2,...
%     'YBinEdges', -2:0.25:2,...
%     'DisplayStyle', 'tile',...
%     'EdgeColor', 'none',...
%     'ShowEmptyBins', 'off');
% 
% colormap(parula)
% colorbar
% clim([1 20]) % adjust as needed
% ccolorbar.Location = ['southoutside'];
% colorbar.Position = [0.3 0.15 0.38 0.03];
% colorbar.Ticks = [1 10 20];

%% Circular mask outside 2.25 Ls
theta = linspace(0,2*pi,600);

% Outer white mask
Rmask = 5;      % larger than plot limits
fill([Rmask*cos(theta) fliplr(2*cos(theta))], ...
    [Rmask*sin(theta) fliplr(2*sin(theta))], ...
    'w','EdgeColor','none');

hold on

% Draw 2.25 Ls boundary
plot(2*cos(theta), 2*sin(theta), ...
    'k--','LineWidth',2);
plot(1*cos(theta), 1*sin(theta), ...
    'k','LineWidth',2);

%% Labels
xlabel('dx')
ylabel('dy')

xlim([-3 3])
ylim([-3 3])
axis equal

grid off          % Remove grid
axis off          % Remove axes, ticks, and labels
hold off