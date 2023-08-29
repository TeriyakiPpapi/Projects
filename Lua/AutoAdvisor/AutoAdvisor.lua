courses = {} -- List of courses on degree path
hours = {} -- The credit hours associated with a course
preqs = {} -- The 3D table that holds the prerequisits for a course
grades = {} -- The grade associated with a course
completed = {} -- A list of courses that have a good standing completion status (not in line with other 4 tables)

gpa = 0 -- The total GPA of the degree path thus far
attemptedHours = 0 -- The amount of hours this degree shows have been tried
completedHours = 0 -- The amount of hours that have been satisfactorally completed
remainingHours = 0 -- The amount of hours on the degree path that have yet to be taken

io.write("File: ") -- Prompt for the file
fileName = io.read("*l") -- Read in a file name
file = io.open(fileName, "r") -- Open the specified file
io.input(file) -- Prepare to take input from the specified file

line = io.read() -- Prime the first line of the file
i = 1
while (line ~= nil) do -- While we have not reached eof
    local parts = {} -- This will hold the separated parts of each line
    for part in (line .. "|") : gmatch("([^|]*)|") do -- Separate the four parts of a line...
        table.insert(parts, part) -- ... and put it into the parts table
    end
    
    -- Trim all leading and trailing whitespace
    for k=1,4 do
        parts[k] = parts[k]:gsub("^%s*(.-)%s*$", "%1")
    end

    table.insert(courses, parts[1]) -- Insert the course to end of table
    table.insert(hours, tonumber(parts[2])) -- Insert the hours to end of table
    table.insert(grades, parts[4] or "") -- Insert the grade to end of table
    
    local j = 1
    preqs[i] = {} -- Prepare first dimension to be written to
    for ors in (parts[3] .. " "):gmatch("([^ ]*) ") do -- Separate groups of courses that are "or'd"
        local k = 1
        preqs[i][j] = {} -- Prepare the second dimension to be written to
        for ands in (ors .. ","):gmatch("([^,]*),") do -- Separate groups of courses that are "anded"
            preqs[i][j][k] = {} -- Prepare the third dimension to be written to
            if ands == "Senior" then -- Check for Senior Standing
                preqs[i][j][k] = "Senior Standing"
            elseif ands == "Standing" then -- Do not write Standing
            elseif ands ~= "" then -- As long as the prerequisit is not empty...
                preqs[i][j][k] = ands -- ... store it
            end -- End if series
            k = k + 1
        end -- End and
        j = j + 1
    end -- End or

    if (grades[i] ~= "" and hours[i] ~= 0) then -- If there is a grade and the course counts for hours
        if grades[i] == "A" then
            gpa = gpa + (4 * hours[i])
            completedHours = completedHours + hours[i]
            table.insert(completed, courses[i])
            
        elseif grades[i] == "B" then
            gpa = gpa + (3 * hours[i])
            completedHours = completedHours + hours[i]
            table.insert(completed, courses[i])

        elseif grades[i] == "C" then
            gpa = gpa + (2 * hours[i])
            table.insert(completed, courses[i])
            completedHours = completedHours + hours[i]

        elseif grades[i] == "D" then
            gpa = gpa + (1 * hours[i])
            remainingHours = remainingHours + hours[i]

        elseif grades[i] == "F" then
            gpa = gpa + (0 * hours[i])
            remainingHours = remainingHours + hours[i]
        end

        attemptedHours = attemptedHours + hours[i] -- Regardless of the grade, the course was attempted
    else -- The course needs to be taken or its hours are zero and will not impact this statement
        remainingHours = remainingHours + hours[i]
    end

    i = i + 1
    line = io.read() -- Prime next line
end
io.close(file) -- Close the file

if attemptedHours ~= 0 then -- If there have been classes attempted
    gpa = gpa / attemptedHours
end

-- Give statistics to user
print(string.format("GPA: %.2f", gpa))
print(string.format("Hours Attempted: %d", attemptedHours))
print(string.format("Hours Completed: %d", completedHours))
print(string.format("Credits Remaining: %d\n", remainingHours))

print("Possible Courses to Take Next")

preqLeft = false -- If there aren't any classes, we want to tell the user
for i, course in ipairs(courses) do -- For all the courses
    if grades[i] == "" -- If this course needs to be taken
    or grades[i] == "D"
    or grades[i] == "F" then
        if type(preqs[i][1][1]) == 'table' then -- If there aren't any prerequisits
            preqLeft = true -- There is a course to take
            print("  " .. courses[i]) -- Recommend a course
        end
        for j, group in ipairs(preqs[i]) do -- Or groups
            canTake = true -- Innocent until proven guilty
            for k, preq in ipairs(group) do -- And groups
                found = false -- Proven innocent status
                for l, done in ipairs(completed) do -- Checking for preq match
                    if preq == done then
                        found = true
                        break
                    end
                end -- End preqs
                if not found then
                    canTake = false
                end
            end -- End And
            ::nopreq:: -- Either there was not a preqrequisite or naturally we checked all the prerequisits
            if canTake then
                preqLeft = true -- There is a course to take
                print("  " .. courses[i]) -- Recommend a course
            end
        end -- End Or
    end -- End grade check
end -- End all courses

-- When there are no more courses left to recommend
if not preqLeft then print("  None - Congratulations!") end
