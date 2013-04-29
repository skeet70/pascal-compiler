echo "" > test_output.pas
for file in Tests/*.mp
do
   echo "" >> test_output.pas
   echo $file >> test_output.pas
   ./mp $file >> test_output.pas
done

for file in Tests/TestsFromPersons/tests/*.mp
do
   echo "" >> test_output.pas
   echo $file >> test_output.pas
   ./mp $file >> test_output.pas
done