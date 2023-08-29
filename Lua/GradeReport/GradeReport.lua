io.write("File: ")
s = io.read("*l") --Get User input
file = io.open(s, "r")
io.input(file)
totalPoints = io.read() --Read the first line

dataSize = 0
name = {}
category = {}
pointsPossible = {}
pointsEarned = {}

for i=1, 1000 do --Loads data into arrays based on lenght
    temp = io.read()
    if temp == nil then
        break
    end
    name[i] = string.sub(temp,1,20) --Trims the string with sub
    category[i] = string.sub(temp,21,40)
    pointsPossible[i] = tonumber(string.sub(temp,41,54))
    pointsEarned[i] = tonumber(string.sub(temp,55,68))
    dataSize = dataSize + 1 --Get the size of my arrays cause I don't want to update my Lua lol
end

io.close(file)

--Next we do some math. Easy Stuff
currentPoints = 0
for i=1, dataSize do
    currentPoints = currentPoints + pointsEarned[i]
end

pointsAvailable = 0
for i=1, dataSize do
    pointsAvailable = pointsAvailable + pointsPossible[i]
end

pointsRemaining = totalPoints - pointsAvailable
currentGrade = ((currentPoints*100) / pointsAvailable)
maxGrade = ((pointsRemaining + currentPoints) * 100 / totalPoints)
minGrade = ((currentPoints * 100) / totalPoints)

--Now it's time for output
--Looks complicated but it's basically just my java code with some minor differences
--You can't do math in an output line, so have mathTemp for that
--To round, use math.floor(variable+.5)
print("")

for y=1, dataSize do
    temp = category[y]
    if temp ~= nil then
        categoryTotal = 0
        categoryCurrent = 0
        categoryWeight = 0
        for x=1, dataSize do
            if category[x]==temp then
                categoryTotal = pointsPossible[x] + categoryTotal
                categoryCurrent = pointsEarned[x] + categoryCurrent
            end
        end
        categoryWeight = ((categoryTotal * 100) / pointsAvailable)
        print(temp.. "(" .. math.floor(categoryWeight+0.5) .. "%)")
        print("==================================")
        for x=1, dataSize do
            if category[x] == temp then
                mathTemp = ((pointsEarned[x] * 100) / pointsPossible[x])
                print(name[x] .. pointsEarned[x] .. "/" .. pointsPossible[x].. "      " ..mathTemp .. "%")
                category[x] = nil
            end
        end
        print("==================================")
        mathTemp = ((categoryCurrent * 100) / categoryTotal)
        print("                    " .. categoryCurrent .."/".. categoryTotal .. "      " ..math.floor(mathTemp+0.5) .. "%")
        print("")
    end
end
print("Current Grade: " .. math.floor(currentGrade+0.5))
print("Minimum Final Grade: " .. math.floor(minGrade+0.5) .. "%")
print("Maximum Final Grade: " .. math.floor(maxGrade+0.5) .. "%")
