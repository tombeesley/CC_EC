function outPat = makeCompoundPattern(tNum, dNum1, dNum2, Trow, Tloc)

global Tpos;
global setTs;
global setCols;

temp = zeros(1,144);

disNum = [dNum1 dNum1 dNum2 dNum2]*1000;

cols = randi(4,4);
locs = randi(4,4)*10;

y = 0:36:108;

while sum(temp>0) ~= 17
    temp(1,:) = 0;
    for q = 1:4
        for d = 1:4
            dPos = Tpos(1); %to enter loop
            while sum(sum(Tpos==dPos)) == 1 % check if distractor is in target location
               dPos = y(q) + randi(36);
            end
            temp(1,dPos) = disNum(d) + locs(q,d) + 1;
        end
    end
        t = setTs(Trow,Tloc) ; %fixed target location and fixed colour
        temp(1,t) = tNum*1000 + 1;
end
    
    outPat = temp;
    
end