function viewPatterns(patIn, mode, order)

Pats = patIn{1,1};

if mode == 1
    set = 1;
    pattern = 1;   
elseif mode == 2   
    pos = 1;   
end
maxSet = size(Pats,1);
maxPat = size(Pats,3);
maxOrder = size(order,1);

% grid coordinates for placing stimuli
x = (70:80:950)+420;
y = 70:80:950;

% screen setup
res=[1920 1080];
midx = res(1)/2;
midy = res(2)/2;

% drawing settings for stims
lineCol = [0 0 0];
backCol = [150 150 150];
penWidth = 4;

% experiment parameters
hardDs = 1; % hard/offset distractors?

% set MainWindow for PTB drawing
MainWindow = Screen('OpenWindow',0, backCol,[0 0 res(1) res(2)]); % a PTB window to draw into

% L stim (drawn at 0 degrees rotation)
Lstim = Screen('OpenOffscreenWindow', MainWindow, backCol, [0 0 60 60]);
if hardDs == 1
    Screen('DrawLine', Lstim, lineCol, 15, 10, 15, 50, penWidth);
else
    Screen('DrawLine', Lstim, lineCol, 10, 10, 10, 50, penWidth);
end
Screen('DrawLine', Lstim, lineCol, 8, 50, 50, 50, penWidth);

% T stim (drawn at 0 degrees rotation)
Tstim = Screen('OpenOffscreenWindow', MainWindow, backCol, [0 0 60 60]);
Screen('DrawLine', Tstim, lineCol, 30, 10, 30, 50, penWidth);
Screen('DrawLine', Tstim, lineCol, 10, 10, 50, 10, penWidth);

% Fixation cross
Fixation = Screen('OpenOffscreenWindow', MainWindow, backCol, [0 0 60 60]);
Screen('DrawLine', Fixation, [255 255 255], 30, 20, 30, 40, penWidth);
Screen('DrawLine', Fixation, [255 255 255], 20, 30, 40, 30, penWidth);

displayPattern = true;

while displayPattern == true

    if mode == 2
        set = order(pos,1);
        pattern = order(pos,2);
    end
    
    %get pattern from Pat array
    curPat = Pats(set,:,pattern);    
    
    Screen('Flip', MainWindow);
    WaitSecs(0.2); %blank screen for 1 sec
    
    % reshape as 12 x 12 grid
    curPat = reshape(curPat,6,24)';
    curPat = [curPat(1:12,:) curPat(13:24,:)];
    % these are used for identifying the target position
    i = reshape(1:36,6,6)';
    locs = [i i+72 ; i+36 i+108]; 

    % display pattern and read keyboard input 
    for i = 1:12
        for k = 1:12
            cellVal = double(curPat(i,k)-1000);
            if cellVal == 1
                % TARGET (random orientation)
                targetLoc = locs(i,k);
                Screen('DrawTexture', MainWindow, Tstim, [], [x(k) y(i) x(k)+60 y(i)+60], 90); % T stim
            elseif cellVal > 0
                % DISRACTOR
                rot = (((cellVal-1)/10)-1)*90; % takes 1-4 and coverts to rotation angle (in 90 degrees)
                Screen('DrawTexture', MainWindow, Lstim, [], [x(k) y(i) x(k)+60 y(i)+60], rot); % L stim
            end
        end
    end
    
    Screen('DrawLine', MainWindow, [255 0 0], 0, midy, res(1), midy, penWidth); % horiztonal line
    Screen('DrawLine', MainWindow, [255 0 0], midx, 0, midx, res(2), penWidth); % vertical line
    
    Screen('DrawText', MainWindow, strcat([int2str(set), '; ', int2str(pattern)]), 200, 200, [200 200 0])
    
    imgOnTime = Screen('Flip', MainWindow); % display image on screen and record time
    
    % GetImage call. Alter the rect argument to change the location of the screen shot
    imageArray = Screen('GetImage', MainWindow);

    % imwrite is a Matlab function, not a PTB-3 function
    patFileName = strcat(['saved_images\', int2str(set), '_', int2str(pattern), '.jpg']);
    imwrite(imageArray, patFileName)
    
    if mode == 1
        
        RestrictKeysForKbCheck([27 37 38 39 40]); % wait for response (c, n, or F11)
        [keyCode, ~, ~] = accKbWait(imgOnTime); % Accurate measure response time, stored as keyDown. If timeout is used specify start time (1) and duration (2).
        
        keyCode = find(keyCode==1);
        
        %[RespKey] = waitkeydown(inf,[52 95 97 98 100]); % wait for arrows or ESC key

        if keyCode == 27
            displayPattern = false;
        end
        if keyCode == 38 %up
            set = set + 1;
            if set > maxSet; set = maxSet; end
        elseif keyCode == 40 %down
            set = set - 1;
            if set == 0; set = 1; end
        elseif keyCode == 39 %right
            pattern = pattern + 1;
            if pattern > maxPat; pattern = maxPat; end
        elseif keyCode == 37 %left
            pattern = pattern - 1;
            if pattern == 0; pattern = 1; end  
        end
        
    elseif mode == 2
        RestrictKeysForKbCheck([27 37 38 39 40]); % wait for response (arrows or escape)
        [keyCode, ~, ~] = accKbWait(imgOnTime);
        keyCode = find(keyCode==1);
        
        if keyCode == 27
            displayPattern = false;
        end
        
        if keyCode == 39 %right
            pos = pos + 1;
            if pos > maxOrder; pos = maxOrder; end
        elseif keyCode == 37 %left
            pos = pos - 1;
            if pos == 0; pos = 1; end  
        end
        
    end
  
    
end

ShowCursor
clear
sca

end
