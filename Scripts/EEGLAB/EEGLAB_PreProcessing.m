clear; close all;

% Set directories for loading and exporting data
rootDir = pwd;
pathIN = fullfile(rootDir, 'studies', 'OKTOS');
pathOUT = fullfile(pathIN, 'derivatives');

% Prompt user to specify if the script should run for all participants or specific list
run_for_all = questdlg('Do you want to run the script for all participants or specify participant IDs?', 'Participant Selection', 'All', 'Specific', 'All');

if strcmp(run_for_all, 'Specific')
    participantIDs = {};
    
    while isempty(participantIDs)
        % Prompt user to specify participant IDs
        participant_ids_input = inputdlg('Enter participant IDs (comma-separated):', 'Participant IDs', [1 50]);

        if isempty(participant_ids_input)
            % Show an error message and ask again
            errordlg('Participant IDs cannot be empty. Please enter participant IDs or click Cancel to exit.', 'Error');
        else
            % Parse user-specified participant IDs
            participantIDs = strsplit(participant_ids_input{1}, ',');
            participantIDs = strtrim(participantIDs);
        end
    end

elseif strcmp(run_for_all, 'All')
    % Get a list of all unique participant IDs
    participantIDs = dir(fullfile(pathIN, 'sub-*'));
    participantIDs = {participantIDs.name};

else
    return; % User cancelled the selection
end

% Prompt user to specify parameters
parameter_prompt = "Please specify the desired pre-processing parameters. " + newline + ...
    "Enter 'Y' to be prompted for each parameter." + newline + ...
    "Enter 'N' if you want to do some research before choosing paremeters." + newline + ...
    "Or, enter 'def' for default parameters." + newline + ...
    "Note: If you press 'Y', you can specify use a combo of specified and default paremeters.";

dims = [1 50];
definput = {''};
parameter_resp = inputdlg(parameter_prompt,'Parameter settings',dims,definput);

if strcmp(parameter_resp,'N')
    return

elseif strcmp(parameter_resp, 'def')
    parameter_dict = struct('HP', 1, 'LP', 50, 'SRATE', 250, 'ICA', 'runica');
else
    specification_prompt = {'Specify high-pass freq value:','Specify low-pass freq value:', ...
        'Specify sampling rate for down-sampling [100/256/512]:', 'Specify ICA algorithm [runica/picard]:'};

    specification_resp = inputdlg(specification_prompt,'Parameter Specification',dims);

    parameter_dict = struct('HP', str2num(specification_resp{1}), ...
        'LP', str2num(specification_resp{2}), ...
        'SRATE', str2num(specification_resp{3}), ...
        'ICA', specification_resp{4});
end

% EEGLAB Load and Set-Up %

% Set EEGLAB path
eeglab_path = fileparts(which('eeglab.m'));

% Open eeglab and store version
[ALLEEG, EEG, CURRENTSET, ALLCOM] = eeglab;

%Change options to store process multiple datasets
pop_editoptions( 'option_storedisk', 0);

% Participant IDs and response status dictionary (0 = non, 1 = resp)
% Define participant IDs and response status (at post-treatment)
IDs = {
    'sub-01', 'sub-02', 'sub-03', 'sub-04', 'sub-05', ...
    'sub-06', 'sub-07', 'sub-08', 'sub-09', 'sub-10', ...
    'sub-11', 'sub-12', 'sub-13', 'sub-14', 'sub-15', ...
    'sub-16', 'sub-17', 'sub-18', 'sub-19', 'sub-20', ...
    'sub-21', 'sub-22', 'sub-23', 'sub-24', 'sub-25', ...
    'sub-26', 'sub-27', 'sub-28', 'sub-29', 'sub-31', ...
    'sub-32', 'sub-33', 'sub-34', 'sub-35', 'sub-36', ...
    'sub-38', 'sub-39', 'sub-40'
};

# Defines response status for baseline (response status for subsequent timepoints changed manually). Note, some subsequently withdrew before post-treatment. 
resp_status = [1,1,0,0,0,1,1,0,0,1,0,0,0,0,1,1,0,1,1,1,1,1,1,1,0,0,0,1,1,1,0,1,1,1,0,1,1,0];

% Create a dictionary by combining participant IDs and response status
global_response_dict = containers.Map(IDs, num2cell(resp_status));

% Extract response status for specified participants
specified_resp_status = values(global_response_dict, participantIDs);

% Create a new dictionary for specified participants
specified_resp_dict = containers.Map(participantIDs, specified_resp_status);

% Create a text file for storing comments
commentFile = fullfile(pathOUT, 'RestingState_Preprocessing.txt');
fid = fopen(commentFile, 'w');
fprintf(fid, 'Data down-sampled to: %d\n', parameter_dict.SRATE);
fprintf(fid, 'Performed FIR high-pass filtering at: %d Hz\n',parameter_dict.HP);
fprintf(fid, 'Performed FIR low-pass filtering at: %d Hz\n', parameter_dict.LP);

% Create table for storing values and set index (updated in participant loop)
variable_types = {'string', 'string', 'datetime', 'string','double', 'double', 'double', 'double', 'double'};
Testing_table = table('Size', [0, 9], 'VariableNames', {'subj_ID', 'tx', 'date', 'type','sampling', 'points', 'resting_duration', 'retained_duration', 'percent_duration'}, 'VariableTypes', variable_types);
index = 1; 

% Loop over each participant ID
for i = 1:length(participantIDs)
    % Store participant ID
    participantID = participantIDs{i};
    disp(participantID)

    fprintf(fid,'------------------------------\n');
    fprintf(fid, 'Resting state preprocessing output for: %s\n ', participantID);

    % Find all EEG files for the current participant ID
    EEG_files = dir(fullfile(pathIN, participantID, '**','*.bdf')); % modify for resting etc.

    % Loop over each file for the current participant ID
    for j = 1:length(EEG_files)
        % Save and print EEG file path
        eeg_path = fullfile(EEG_files(j).folder, EEG_files(j).name);
        fprintf(fid, 'Loaded EEG data from file: %s\n', eeg_path);

        % Store participant specific data/directories/files paths
        % Timepoint (i.e. ses-#)
        timepoint = string(extractBetween(EEG_files(j).folder,[participantID, '\'] ,"\eeg"));

        % Task type
        task = regexp(eeg_path, '(EC|EO)', 'match', 'once');
        
        % Set timepoint specific output folder
        parti_pathOUT = convertStringsToChars(fullfile(pathOUT, participantID, timepoint, 'eeg'));
        
        % EEGLAB Processing
        % Load the EEG to EEGLAB
        EEG = pop_biosig(eeg_path, 'channels', 1:32 ,'ref', 32); % Cz reference

        % Store dataset as new dataset
        [ALLEEG, EEG, CURRENTSET] = eeg_store( ALLEEG, EEG);

        % Set EEG fields
        EEG.filepath = eeg_path;
        EEG.subject = participantID;
        EEG.session = timepoint;
        EEG.group = specified_resp_dict(participantID);
        EEG.condition = task;

        % Perform dataset consistency check
        EEG = eeg_checkset( EEG );
        
        % Set channel locations from default file (Opt. head center??)
        EEG = pop_chanedit(EEG, 'lookup', 'C:\Users\Bonnie\Documents\eeglab2023.0\plugins\dipfit\standard_BEM\elec\standard_1005.elc'); 

        % Select resting data portion using event latencies (in seconds)

        % Check if the number of EEG events is not equal to 3
        if length(EEG.event) ~= 3
            % Write an error message to the comment file
            fprintf(fid, 'Error: Number of EEG events is not equal to 3. File not processed.\n');

            % Skip the current loop iteration
            continue;
        end

        % Calculate resting_start and resting_end if there are 3 EEG events
        resting_start = ([EEG.event(2).latency]-1)/EEG.srate;
        resting_end = ([EEG.event(3).latency]-1)/EEG.srate;
        total_time = resting_end - resting_start;
        EEG = pop_select(EEG, 'time', [resting_start, resting_end]);

        % Down sample from 1028Hz (maybe move before filtering?)
        EEG = pop_resample( EEG, parameter_dict.SRATE);

        % Re-reference, keeping full rank
        EEG = fullRankAveRef(EEG);

        % Perform FIR high-pass filtering
        EEG = pop_eegfiltnew(EEG, parameter_dict.HP, [], [], 0);
        [ALLEEG EEG CURRENTSET] = eeg_store(ALLEEG, EEG, CURRENTSET);

        % The following cleans line noise 
        % if Low-pass >= 50 is selected if parameter_dict.LP >= 50 
        EEG = pop_zapline_plus(EEG, 'noisefreqs','line','coarseFreqDetectPowerDiff',4,'chunkLength',0,'adaptiveNremove',1,'fixedNremove',1,'plotResults',0);

        EEG = eeg_checkset( EEG );

        EEG = pop_cleanline(EEG, 'bandwidth',2,'chanlist',[1:EEG.nbchan] ,'computepower',1,'linefreqs',50,'newversion',0,'normSpectrum',0,'p',0.01,'pad',2,'plotfigures',0,'scanforlines',0,'sigtype','Channels','taperbandwidth',2,'tau',100,'verb',0,'winsize',4,'winstep',1);
       
        % Perform dataset consistency check
        EEG = eeg_checkset( EEG );

        % Re-reference, keeping full rank after channel removal
        EEG = fullRankAveRef(EEG);

        % Perform FIR low-pass filtering
        EEG = pop_eegfiltnew(EEG, 'hicutoff', parameter_dict.LP,'plotfreqz',1);

        % Keep original EEG for channel interpolation
        originalEEG = EEG;

        % Perform dataset consistency check
        EEG = eeg_checkset( EEG );

        % Compute ICA
        EEG = pop_runica(EEG, 'icatype', parameter_dict.ICA, 'extended',1,'interrupt','off');
        
        % run ICLabel and save IC scalp topographies and labels
        EEG = pop_iclabel(EEG, 'default');

        % Save IC scalp topographies and labels
        IC_length = length (EEG.chaninfo.icachansind);
        pop_viewprops( EEG, 0, 1:IC_length, {}, {}, 1, 'ICLabel' );
        comp_plot_name = [task, '_comp_label_plots'];
        saveas(gcf, fullfile(parti_pathOUT, comp_plot_name), 'png');
        close;

        % Flag ICA components 
        EEG = pop_icflag(EEG,[NaN NaN;0.8 1;0.8 1;0.8 1;0.8 1;0.8 1;NaN NaN]);

        % Remove components marked for rejection
        EEG = pop_subcomp( EEG, [], 0);

        % Perform dataset consistency check
        EEG = eeg_checkset( EEG );
        [ALLEEG EEG] = eeg_store(ALLEEG, EEG, CURRENTSET);

        % Re-reference, keeping full rank after channel removal
        EEG = pop_reref(EEG,[]);

        % Plot and save PSD
        figure; pop_spectopo(EEG, 1, [0  239912], 'EEG' , 'freq', [6 10 30], 'freqrange',[2 60],'electrodes','off');
        spectra_plot_name = [task, '_chan_spectra'];
        saveas(gcf, fullfile(parti_pathOUT, spectra_plot_name), 'jpeg');
        close;
        
        % Clean raw data (Note: ASR interpolation used!)
        EEG = pop_clean_rawdata(EEG, 'FlatlineCriterion',5,'ChannelCriterion',0.8,'LineNoiseCriterion',4,'Highpass','off','BurstCriterion',20,'WindowCriterion',0.25,'BurstRejection','off','Distance','Euclidian','WindowCriterionTolerances',[-Inf 7] );
    
        EEG = pop_interp(EEG, originalEEG.chanlocs, 'spherical');

        % Re-reference, keeping full rank after channel removal
        EEG = pop_reref(EEG,[]);
        
        % Perform dataset consistency check
        EEG = eeg_checkset( EEG );

        % Compute surface laplacian
        EEG = pop_currentdensity(EEG, 'method','spline');
        
        [ALLEEG EEG] = eeg_store(ALLEEG, EEG, CURRENTSET);

        % Store current set and set name before exporting
        EEG.setname = EEG_files(j).name;
        
        % Save channel and component data for analysis using pop_export() --> ensure full paths are specified as characters

        % .mat files
        chan = convertStringsToChars(fullfile(parti_pathOUT,[task, '_chan_activity.mat']));
        %comp = convertStringsToChars(fullfile(parti_pathOUT,[task,'_comp_activity.mat']));
        pop_export(EEG, chan,'separator',',', 'time','off','precision',10);
        %pop_export(EEG, comp,'ica','on','separator',',','time','off','precision',10);

        % .csv files
        chan_csv = convertStringsToChars(fullfile(parti_pathOUT,[task,'_chan_activity.csv']));
        %comp_csv = convertStringsToChars(fullfile(parti_pathOUT,[task,'_comp_activity.csv']));
        pop_export(EEG, chan_csv,'separator',',','time','off','precision',10);
        %pop_export(EEG, comp_csv,'ica','on','separator',',','time','off','precision',10);
        
        % Save output
        pop_saveset(EEG, 'filename', [char(participantID), '_', char(timepoint), '_', char(task), '_preprocessed.set'], 'filepath', parti_pathOUT);
        fprintf(fid, 'Saved EEG data to .set file here: %s\n', parti_pathOUT);

        % Plot and save channel activations scroll for visual check
        %h(1) = figure ('name', eeg_plot_name);
        pop_eegplot( EEG, 1, 1, 1);
        eeg_plot_name = [task, '_eeg_scroll'];
        saveas(gcf, fullfile(parti_pathOUT, eeg_plot_name), 'jpeg');
        close;

        % Plot and save channel locations 2-D
        % chanlocs_name = 'chanlocs';
        % topoplot([],EEG.chanlocs, 'style', 'blank',  'electrodes', 'labelpoint', 'chaninfo', EEG.chaninfo);
        % saveas(gcf, fullfile(parti_pathOUT, chanlocs_name), 'jpeg');
        % close;

        % Save date/time EEG file was created
        dateInfo = EEG.etc.T0;
        dt = datetime(dateInfo(1), dateInfo(2), dateInfo(3), dateInfo(4), dateInfo(5), dateInfo(6));

        % Store values to add to Output Table
        structTesting(index, 1).subj_ID = participantID;
        structTesting(index, 1).tx = timepoint;
        structTesting(index, 1).date = dt;
        structTesting(index, 1).type = task;
        structTesting(index, 1).sampling = parameter_dict.SRATE;
        structTesting(index, 1).points = EEG.pnts;
        structTesting(index, 1).resting_duration = total_time;
        structTesting(index, 1).retained_duration = EEG.xmax;
        structTesting(index, 1).percent_duration = (EEG.xmax/total_time)*100;

        index = index + 1;

        eeglab redraw;

    end
    eeglab redraw;
end

% Create and save table with key processing data 
Processing_table = [Testing_table; struct2table(structTesting)];
writetable(Processing_table,fullfile(pathOUT, 'RestingEEG_Preprocessing.csv'),'WriteRowNames',true)

% Close the comment file
fclose(fid);

eeglab redraw % Redraw the main EEGLAB window