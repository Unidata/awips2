package ohd.hseb.util.gui.jtable.example;

import ohd.hseb.util.gui.jtable.AbstractJTableRowData;
import ohd.hseb.util.gui.jtable.BaseTableCell;
import ohd.hseb.util.gui.jtable.CellType;

/**
 * This class is mandate for any application trying to use JTable Framework.
 * It should extend AbstractJTableRowData class from common project's jtable pacakge.
 * It contains the getters and setters for each column in the table.
 * It specifies the cell level details for each column in the table.
 * It should have a method similar to the addAllCellsToMap() shown here, which will create the 
 * cells and map the details to the cells like its value, data type etc.
 *  
 * @author rajaramv
 *
 */
public class ExampleJTableRowData extends AbstractJTableRowData
{
    String _missingRepresentation = " ";
    private int _id;
    private short _age;
    private long _dateOfBirth;
    private long _salary;
    private float _height;
    private double _weight;
    private String _name;
    private boolean _maritalStatus;
    
    public void setMaritalStatus(boolean value)
    {
        _maritalStatus = value;
    }
    public boolean getMaritalStatus()
    {
        return _maritalStatus;
    }
    public double getWeight()
    {
        return _weight;
    }
    public void setWeight(double value)
    {
        _weight = value;
    }
    public float getHeight()
    {
        return _height;
    }
    public void setHeight(float value)
    {
        _height = value;
    }
    public int getId()
    {
        return _id;
    }
    public void setId(int value)
    {
        _id = value;
    }
    public long getBirthDate()
    {
        return _dateOfBirth;
    }
    public void setBirthDate(long value)
    {
        _dateOfBirth = value;
    }
    public short getAge()
    {
        return _age;
    }
    public void setAge(short value)
    {
        _age = value;
    }
    public long getSalary()
    {
        return _salary;
    }
    public void setSalary(long value)
    {
        _salary = value;
    }
    public String getName()
    {
        return _name;
    }
    public void setName(String value)
    {
        _name = value;
    }
    
    /**
     * Creates the individual cells and specifies its properties, like data tpe, value etc.
     *
     */
    public void addAllCellsToMap()
    {
        addCell(new BaseTableCell("Id", CellType.INTEGER, getId(), _missingRepresentation));
        addCell(new BaseTableCell("Age", CellType.SHORT, getAge(), _missingRepresentation));
        addCell(new BaseTableCell("BirthDate", CellType.DATE, getBirthDate(), _missingRepresentation));
        addCell(new BaseTableCell("Height", CellType.FLOAT, getHeight(), _missingRepresentation));
        addCell(new BaseTableCell("Weight", CellType.DOUBLE, getWeight(), _missingRepresentation));
        addCell(new BaseTableCell("Name", CellType.STRING, getName(), _missingRepresentation));
        addCell(new BaseTableCell("Married", CellType.BOOLEAN, getMaritalStatus(), _missingRepresentation));
        addCell(new BaseTableCell("Salary", CellType.LONG, getSalary(), _missingRepresentation));
    }
}
