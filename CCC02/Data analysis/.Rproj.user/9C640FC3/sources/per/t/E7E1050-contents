function outData = makePats(blocks,subjects,NumSessions)

RandStream.setGlobalStream(RandStream('mt19937ar','seed',sum(100*clock)));

outData = cell(subjects,2);
global Tpos; Tpos = [10 11 15 20 26; 44 50 57 64 65; 80 81 88 95 101; 119 125 130 134 135];
global setTs;
global setCols;
global P;
global Dlocs;
Dlocs = zeros(4,31);
Dlocs(1,:) = [1:9 12:14 16:19 21:25 27:36];
Dlocs(2,:) = [37:43 45:49 51:56 58:63 66:72];
Dlocs(3,:) = [73:79 82:87 89:94 96:100 102:108];
Dlocs(4,:) = [109:118 120:124 126:129 131:133 136:144];

global Dnums;
Dnums = repmat([2000 3000 4000 5000],4,1);

for sub = 1:subjects  
    
    setTs = zeros(2,4);
    for i = 1:4
        temp = Tpos(i,randperm(5));
        setTs(:,i) = temp(1:2); %sets the locations of the targets in fixed patterns
    end
    setCols = zeros(2,4);
    for i = 1:2
        setCols(i,:) = randperm(4)'; %sets the colours of the targets in fixed patterns
    end
    
    %make pattern array
    P = zeros(12,144,4*blocks,NumSessions);

    clc; sub 
   
    
    % Repeating patterns (main task)
    for i = 1:4
        P(1,:,i,1) = makeCompoundPattern(1,1,1,1,i);
    end
    P(1,:,1:4*blocks,1) = repmat(P(1,:,1:4,1),[1 1 blocks 1]);
   
    %Random patterns   (main task) 
    for x = 0:4:4*blocks-4
        for i = 1:4
            P(2,:,x+i,1) = makeCompoundPattern(1,1,1,1,i);
        end
    end
    
    % New repeating patterns (stim type 3)
    for i = 1:4
        P(3,:,i,1) = makeCompoundPattern(1,1,1,1,i);
    end
    P(3,:,1:4*blocks,1) = repmat(P(3,:,1:4,1),[1 1 blocks 1]);
    
    
    % ordering
    order = zeros(320,6);
    
    order(1:320,:) = OrderTrials([1 2],1,40,0); % main task
    
    outData(sub,1) = {uint16(P)};
    outData(sub,2) = {uint16(order)};
    outData(sub,3) = {setTs};
    outData(sub,4) = {setCols};
    
end