# Find functions in a script

FILES='ls *.R'
for file in $($FILES)
do
  echo $file
  grep "Routine:" $file | sed 's/\# Routine: /   /g'
done




