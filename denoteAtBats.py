###This is python that i used to help denote unique at bats,
### I tried in R, but it was quite slow
import fileinput
import sys
oldBatter = ""
newBatter = ""
lastPitch = ""
sys.stdout.write("last.pitch, at.bat.pitch.count")
sys.stdout.write("\n")
for line in fileinput.input():
    if not fileinput.isfirstline():
        row = line.split(",")
        newBatter = row[1]
        if (newBatter == oldBatter):
            count = count + 1
            sys.stdout.write(row[7] + "," + str(count))
            sys.stdout.write("\n")

        else:
            count = 1
            sys.stdout.write("NA," +str(count))
            sys.stdout.write("\n")

        oldBatter = newBatter
