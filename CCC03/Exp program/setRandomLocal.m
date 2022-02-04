function outPat = setRandomLocal(inputPat, dNum1, dNum2, targetQuad)

global Tpos;

disNum = [dNum1 dNum1 dNum2 dNum2]*1000;

locs = randi(4,4)*10; % this is rotation of distractors

quadInd = [1:36; 37:72; 73:108; 109:144]; % indices of quadrants

temp = inputPat;

pat_tpos = find(temp==1001); % get target position

temp(quadInd(targetQuad,:)) = 0; % set quadrant to zero

temp(pat_tpos) = 1001; % add target back in

y = 0:36:108;

while sum(temp>0) ~= 17
    temp(quadInd(targetQuad,:)) = 0; % set quadrant to zero
    temp(pat_tpos) = 1001; % add target back in
    for q = targetQuad % just adds distractors to the target quadrant
        for d = 1:4
            dPos = Tpos(1); %to enter loop
            while sum(sum(Tpos==dPos)) == 1 % check if distractor is in target location
               dPos = y(q) + randi(36);
            end
            temp(1,dPos) = disNum(d) + locs(q,d) + 1;
        end
    end
end
    
outPat = temp;
    
end