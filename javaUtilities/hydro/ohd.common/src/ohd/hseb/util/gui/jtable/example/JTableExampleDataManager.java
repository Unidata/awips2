package ohd.hseb.util.gui.jtable.example;

import java.util.ArrayList;
import java.util.List;

/**
 * This class's primary repsonsibility is the create the list of rowdata objects,
 * and call addAllCellsToMap() method (Declared in ExampleJTableRowData.java)
 * for each row data object.
 * 
 * @author rajaramv
 *
 */
public class JTableExampleDataManager
{

    private int _numOfRows = 10;
    private ExampleData _exampleData = new ExampleData();
   
    public List readData()
    {
        List rowDataList = new ArrayList();
     
        int ids[] = _exampleData.getExampleIdValues();
        short ages[] = _exampleData.getExampleAgeValues();
        long dateOfBirths[] = _exampleData.getExampleDateOfBirthValues();
        long salaries[] = _exampleData.getExampleSalaryValues();
        float heights[] = _exampleData.getExampleHeightValues();
        double weights[] = _exampleData.getExampleWeightValues();
        boolean maritalStatus[] = _exampleData.getExampleMaritalStatusValues();
        String names[] = _exampleData.getExampleNameValues();

        for(int i=0; i < _numOfRows ; i++)
        {
            ExampleJTableRowData rowData = new ExampleJTableRowData();
            rowData.setId(ids[i]);
            rowData.setAge(ages[i]);
            rowData.setHeight(heights[i]);
            rowData.setWeight(weights[i]);
            rowData.setBirthDate(dateOfBirths[i]);
            rowData.setMaritalStatus(maritalStatus[i]);
            rowData.setName(names[i]);
            rowData.setSalary(salaries[i]);
            rowData.addAllCellsToMap();
            rowDataList.add(rowData);
        }
        return rowDataList;
   }
    
}
