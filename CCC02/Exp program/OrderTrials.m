function out = OrderTrials(TT,startB,blocks,lastPref)

TPB = numel(TT)*4; %trials per block
bOrder = zeros(TPB*blocks,6); % array that holds current randomisation of trials

for b = 1:blocks
   
        step = (b-1)*TPB; tStep = (startB+b-2)*4; %used for array indexing
        
        temp1 = reshape(repmat(TT,4, 1),TPB,1); % trial types
        temp2 = reshape(repmat(tStep+1:tStep+4,1,numel(TT)),TPB,1); % pattern index
        temp3 = reshape(repmat(1:4,1,numel(TT)),TPB,1); % target quadrant
        temp = [temp1 temp2 temp3];
        
        check = 0;        
        while check == 0
            
            x = randperm(TPB); %random order of trials
            bOrder(step+1:step+TPB,4:6) = temp(x,:);

            check = 1;
            if min(abs(diff(bOrder(step+1:step+TPB,6)))) == 0
                check = 0; %checks for repeating targets within blocks
            end
            if b > 1 && bOrder(step+1,6) == bOrder(step,6)
                check = 0;
            end
            if bOrder(1,6) == lastPref
                check = 0; % check for repeating targets across stages
            end
        end
        
        %adds remaining trial details
        temp4 = ones(TPB,1); % session number
        temp5 = (startB+b-1)*ones(TPB,1); % block number
        temp6 = (1:TPB)'; % trial number
        bOrder(step+1:step+TPB,1:3) = [temp4 temp5 temp6];
    
end

out = bOrder; %output randomisation back to main array.
