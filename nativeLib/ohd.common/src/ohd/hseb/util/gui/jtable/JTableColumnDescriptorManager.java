package ohd.hseb.util.gui.jtable;

import java.util.*;

public class JTableColumnDescriptorManager implements JTableColumnDisplaySettings
{
    private List<JTableColumnDescriptor> _columnDescriptorList = null;
    private Map<String, JTableColumnDescriptor> _columnDescriptorMap = null;
    private List _columnDescriptorForSortInfoList = null;
    private static final int _totalSortToRecord = 4;
    private Map _columnSettingMap = null;
    

    public JTableColumnDescriptorManager(List columnDescriptorList)
    {
        _columnDescriptorList = columnDescriptorList;
        _columnDescriptorMap = new HashMap();
        for(int i =0 ; i < _columnDescriptorList.size(); i++)
        {
            JTableColumnDescriptor columnDescription = (JTableColumnDescriptor) 
            _columnDescriptorList.get(i);
            _columnDescriptorMap.put(columnDescription.getColumnName(), 
                columnDescription);
        }  

        _columnDescriptorForSortInfoList = new ArrayList(_totalSortToRecord);

    }

    public String getSettingsString() 
    {
        String columnDisplaySetting[] = getDisplayedColumnSettings();
        String columnSortSetting[] = getColumnSortSettings();
        String settings = null;

        settings = "#COLUMN:Column Name|Column Width".concat("\n");
        if(columnDisplaySetting != null)
        {
            for(int i=0 ; i < columnDisplaySetting.length; i++)
            {
                settings = settings.concat(columnDisplaySetting[i]).concat("\n");
            }
        }
        settings = settings.concat("\n").concat("#SORT:Column Name|IsAscending|Sort Order").concat("\n");
        if(columnSortSetting != null)
        {
            for(int i=0; i < columnSortSetting.length; i++)
            {
                settings = settings.concat(columnSortSetting[i]).concat("\n");
            }
        }
        return settings;
    }

    public List<String> getSettingsListFromSettingsString(String settings)
    {
        List<String> returnList = new ArrayList<String>();
        int begIndex = 0;
        while(settings != null)
        {
            int indexOfNewLine = settings.indexOf("\n");
            if(indexOfNewLine != -1)
            {
                String line = settings.subSequence(begIndex, indexOfNewLine).toString();
                returnList.add(line);
                settings = settings.subSequence(indexOfNewLine+1, settings.length()).toString();
            }
            else
            {
                break;
            }
        }

        return returnList;
    }

    public List<String> getTableSettingsForTokenMentioned(List<String> settingsList, String tokenName)
    {
        List<String> settingsListForTokenMentioned = new ArrayList<String> (); 
        if(settingsList != null)
        {
            for(int i=0; i < settingsList.size(); i++)
            {
                String tempStr = settingsList.get(i).toString();
                if(!(tempStr.equals("")))
                {
                    String str = tempStr.subSequence(0,tokenName.length()).toString();
                    if(str.compareTo(tokenName) == 0)
                    {
                        settingsListForTokenMentioned.add(settingsList.get(i));
                    }
                }
            }

        }
        return settingsListForTokenMentioned;
    }

    public void setSettingsFromString(String settings) 
    {
        String header = "JTableColumnDescriptorManager.setSettingsFromString(): ";
       
        if(settings != null)
        {
            _columnDescriptorForSortInfoList.clear();  //make the list empty

            List<String> settingsList = getSettingsListFromSettingsString(settings);

            boolean isColumnDisplayTokenReadAndSet = readColumnDisplayToken(settingsList);
            if(isColumnDisplayTokenReadAndSet)
            {
                readColumnSortToken(settingsList);
            }
        }
    }
    
    private void readColumnSortToken(List<String> settingsList)
    {
        String columnSortToken = "SORT:";
        String str;
        
        List<String> columnNamesToSortList = new ArrayList<String>();
        List<Boolean> columnSortDirectionList = new ArrayList<Boolean>();
        List<Integer> columnSortOrderList = new ArrayList<Integer>();
        
        List<String> stringListForSortToken = getTableSettingsForTokenMentioned(settingsList, columnSortToken);
        
        if(stringListForSortToken != null)
        {
            for(int i =0; i < stringListForSortToken.size(); i++)
            {
                str = stringListForSortToken.get(i).toString();
                str = str.substring(columnSortToken.length(), str.length()).trim();
              
                String[] splitStr = str.split("\\|");
                if(splitStr != null && splitStr.length == 3)
                {
                    if(isValidColumn(splitStr[0]))
                    {
                        columnNamesToSortList.add(splitStr[0]);

                        boolean columnSortAsc = Boolean.parseBoolean(splitStr[1]);
                        columnSortDirectionList.add(columnSortAsc);

                        columnSortOrderList.add(Integer.parseInt(splitStr[2]));
                    }
                }
            }   
    
            for(int i=0; i < columnNamesToSortList.size(); i++)
            {
                selectColumnForSorting(columnNamesToSortList.get(i), columnSortDirectionList.get(i),
                    i, false);
            }
        }
    }
    
    private boolean readColumnDisplayToken(List<String> settingsList)
    {
        String columnDisplayToken = "COLUMN:";
        String str;
        boolean isColumnDisplayTokenReadAndSet = false;
        
        List<String> columnNamesToDisplayList = new ArrayList<String>();
        List<Integer> columnWidthList = new ArrayList<Integer>();
        
        String columnNamesToDisplay[] = null;
        int columnWidths[] = null;
      
        List<String> stringListForColumnDisplayToken =  getTableSettingsForTokenMentioned(settingsList, columnDisplayToken);
       
        if(stringListForColumnDisplayToken != null)
        {
            for(int i =0; i < stringListForColumnDisplayToken.size(); i++)
            {
                str = stringListForColumnDisplayToken.get(i).toString();
                str = str.substring(columnDisplayToken.length(), str.length()).trim();
                String[] splitStr = str.split("\\|");
                if(splitStr != null && splitStr.length == 2)
                {
                    if(isValidColumn(splitStr[0]))
                    {
                        columnNamesToDisplayList.add(splitStr[0]);
                        int columnWidth = Integer.parseInt(splitStr[1]);
                        columnWidthList.add(columnWidth);
                    }
                }
            }
            columnNamesToDisplay = new String[columnNamesToDisplayList.size()];
            columnWidths = new int[columnNamesToDisplayList.size()];

            for(int i=0; i < columnNamesToDisplayList.size(); i++)
            {
                columnNamesToDisplay[i] = columnNamesToDisplayList.get(i);
                columnWidths[i] = columnWidthList.get(i);
            }
        }
  
        if(columnNamesToDisplay!= null )
        {
            setDisplayableColumns(columnNamesToDisplay);
            setWidthOfDisplayedColumns(columnWidths);
            isColumnDisplayTokenReadAndSet = true;
        }

        return isColumnDisplayTokenReadAndSet;
    }

    protected boolean isValidColumn(String columnName)
    {
        boolean isValidColumn = false;
        Set validColumnNamesSet = getSetOfValidColumnNames();

        if( validColumnNamesSet.contains(columnName) )
        {
            isValidColumn = true;
        }

        if( ! isValidColumn)
        {
            System.out.println("Invalid column name ["+ columnName +"] in settings file");
            System.out.println("Check the settings file...");
        }
        return isValidColumn;
    }

    protected String[] getAllowedColumnNamesArray()
    {
        String[] allowedColNames = new String[_columnDescriptorList.size()];

        for(int i = 0; i < allowedColNames.length ; i++)
        {
            JTableColumnDescriptor columnDescriptor;
            columnDescriptor = (JTableColumnDescriptor) _columnDescriptorList.get(i);
            allowedColNames[i] = columnDescriptor.getColumnName();
        }

        return allowedColNames; 
    }

    protected Set getSetOfValidColumnNames()
    {
        // String header = "JTableColumnDescriptorManager.getSetOfValidColumnNames(): ";
        Set validColumnNamesSet = new HashSet();

        JTableColumnDescriptor columnDescription = null;
        for(int j=0; j < _columnDescriptorList.size() ;j++)
        {
            columnDescription = (JTableColumnDescriptor) _columnDescriptorList.get(j);
            validColumnNamesSet.add(columnDescription.getColumnName());
            // System.out.println(header + "columnName[" + j +"] = " + columnDescription.getColumnName());
        }
        return validColumnNamesSet;
    }
    
 
    protected JTableColumnDescriptor getColumnDescriptorByName(String columName)
    {
        return _columnDescriptorMap.get(columName);
    }

    protected void setDisplayableColumns(String columnNamesSelected[])
    {
        for(int i=0;i < _columnDescriptorList.size(); i++)
        {
            JTableColumnDescriptor columnDescription = new JTableColumnDescriptor();
            columnDescription = (JTableColumnDescriptor) _columnDescriptorList.get(i);
            columnDescription.setDisplay(false);
            for(int j=0; j < columnNamesSelected.length ;j++)
            {
                if( columnDescription.getColumnName().compareTo(columnNamesSelected[j]) == 0 )
                {
                    columnDescription.setDisplay(true);
                }
            }
        }

        //Rearrange the list in such a way that the descriptor of selected column names
        //Appear in the top of the list, and they are added in the same order as
        //the selected column names
        JTableColumnDescriptor columnDescription = new JTableColumnDescriptor();

        for(int i = 0 ; i < columnNamesSelected.length; i++)
        {
            columnDescription = (JTableColumnDescriptor) _columnDescriptorMap.
            get(columnNamesSelected[i]);
            if(columnDescription != null)
            {
                _columnDescriptorList.remove(columnDescription);
                _columnDescriptorList.add(i, columnDescription);
            }
        }
  
    }

    protected List getColumnDescriptorList()
    {
        return _columnDescriptorList;
    }

    protected String[] getAllColumnNamesInColumnDescriptorManager()
    {
        String columnNames[] = new String[_columnDescriptorList.size()];
        if(_columnDescriptorList.size() > 0)
        {
            columnNames = new String[_columnDescriptorList.size()];
            for(int i=0;i < _columnDescriptorList.size(); i++)
            {
                JTableColumnDescriptor columnDescription = new JTableColumnDescriptor();
                columnDescription = (JTableColumnDescriptor) _columnDescriptorList.get(i);
                columnNames[i] = columnDescription.getColumnName();
            }
        }

        return columnNames;
    }

    protected String[] getColumnNamesThatAreCurrentlyDisplayed()
    {
        String columnNames[] = null;
        int cnt=0;
        JTableColumnDescriptor columnDescription;

        for(int i=0;i < _columnDescriptorList.size(); i++)
        {
            columnDescription = (JTableColumnDescriptor) _columnDescriptorList.get(i);
            if ( columnDescription.getDisplay())
                cnt++;
        }
        if(cnt > 0)
        {
            columnNames = new String[cnt];
            int j=0;
            for(int i=0;i < _columnDescriptorList.size(); i++)
            {
                columnDescription = (JTableColumnDescriptor) _columnDescriptorList.get(i);
                if ( columnDescription.getDisplay())
                {
                    columnNames[j++] = columnDescription.getColumnName();

                }
            }
        }
        return columnNames;
    }

    protected void swapColumnsInList(String columnName1, String columnName2)
    {
        JTableColumnDescriptor columnDescriptor1,  columnDescriptor2;
        columnDescriptor1 = (JTableColumnDescriptor) _columnDescriptorMap.
        get(columnName1);
        columnDescriptor2 = (JTableColumnDescriptor) _columnDescriptorMap.
        get(columnName2);

        int index1 = _columnDescriptorList.indexOf(columnDescriptor1);
        int index2 = _columnDescriptorList.indexOf(columnDescriptor2);

        _columnDescriptorList.set(index1, columnDescriptor2);
        _columnDescriptorList.set(index2, columnDescriptor1);

    }

    protected JTableColumnDisplaySettings getTableColumnSettings()
    {
        return this;
    }

    protected void clearSort()
    {
        String header = "JTableColumnDescriptorManager.clearSort(): ";
        if(_columnDescriptorForSortInfoList != null)
        {
            _columnDescriptorForSortInfoList.clear();
        }
    }
    
    protected void setSortInfoNotBasedOnUserClicks(String columnNamesToSort[], boolean columnSortOrder[], int columnSortNumber[])
    {
        setSortOrderOfColumns(columnSortOrder, columnNamesToSort, columnSortNumber);

        for(int i=0; i < columnSortNumber.length; i++)
        {
            if(columnSortNumber[i] != -1)
            {
                selectColumnForSorting(columnNamesToSort[i], columnSortOrder[i],
                    columnSortNumber[i], false);
            }
        }

    }

    protected String[] getDisplayedColumnSettings()
    {
        String columnNamesInDisplay[] = getColumnNamesThatAreCurrentlyDisplayed();

        String columnSetting[] = new String[columnNamesInDisplay.length];

        JTableColumnDescriptor columnDescriptor;
        int cnt=0;
        for(int i=0 ; i < _columnDescriptorList.size(); i++)
        {
            columnDescriptor = (JTableColumnDescriptor) _columnDescriptorList.get(i);
            if(columnDescriptor.getDisplay())
            {
                String columnName = columnDescriptor.getColumnName();
                int columnWidth = columnDescriptor.getWidth();
                columnSetting[cnt++] = "COLUMN:"+columnName+"|"+columnWidth;
            }
        }

        return columnSetting;
    }

    protected int[] getWidthOfDisplayedColumns()
    {
        String columnsDisplayed[] = getColumnNamesThatAreCurrentlyDisplayed();
        int columnWidths[] = new int[columnsDisplayed.length];


        JTableColumnDescriptor columnDescriptor = null;
        for(int i=0; i < columnsDisplayed.length; i++)
        {
            columnDescriptor = (JTableColumnDescriptor)  _columnDescriptorMap.get(columnsDisplayed[i]);
            columnWidths[i] = columnDescriptor.getWidth();
        }

        return columnWidths;
    }

    protected boolean[]  getIsEditableArray()
    {
        String columnsDisplayed[] = getColumnNamesThatAreCurrentlyDisplayed();
        boolean[] isEditableArray = new boolean[columnsDisplayed.length];
        
        JTableColumnDescriptor columnDescriptor = null;
        for(int i=0; i < columnsDisplayed.length; i++)
        {
            columnDescriptor = (JTableColumnDescriptor)  _columnDescriptorMap.get(columnsDisplayed[i]);
            isEditableArray[i] = columnDescriptor.isEditable();
        }
             
        return isEditableArray;     
    }
    
    protected String[] getAlignmentOfDisplayedColumns()
    {
        String columnsDisplayed[] = getColumnNamesThatAreCurrentlyDisplayed();
        String columnAlignments[] = new String[columnsDisplayed.length];

        JTableColumnDescriptor columnDescriptor;
        for(int i=0; i < columnsDisplayed.length; i++)
        {
            columnDescriptor = (JTableColumnDescriptor)
            _columnDescriptorMap.get(columnsDisplayed[i]);
            columnAlignments[i] = columnDescriptor.getAlignment();
        }

        return columnAlignments;
    }

    protected boolean[] getSortOrderOfAllColumns()
    {
        String allColumns[] = getAllColumnNamesInColumnDescriptorManager();

        boolean columnsAscOrDescArray[] = new boolean[allColumns.length];

        JTableColumnDescriptor columnDescriptor;
        for(int i=0; i < allColumns.length; i++)
        {
            columnDescriptor = (JTableColumnDescriptor)
            _columnDescriptorMap.get(allColumns[i]);
            columnsAscOrDescArray[i] = columnDescriptor.getSortAsc();
        }
        return columnsAscOrDescArray;
    }

    protected int getSortOrderNumberForColumn(String columnName)
    {
        int columnSortOrder = -1;

        JTableColumnDescriptor columnDescriptor = (JTableColumnDescriptor)
        _columnDescriptorMap.get(columnName);
        columnSortOrder = columnDescriptor.getSortOrder();
        return columnSortOrder;
    }

    protected boolean getSortDirectionForColumn(String columnName)
    {
        boolean isAscending = false;

        JTableColumnDescriptor columnDescriptor = (JTableColumnDescriptor) _columnDescriptorMap.get(columnName);
        isAscending = columnDescriptor.getSortAsc();
        
        return isAscending;
    }

    protected void setWidthOfDisplayedColumns(int columnWidths[])
    {
        String columnsDisplayed[] = getColumnNamesThatAreCurrentlyDisplayed();

        JTableColumnDescriptor columnDescriptor;
        for(int i=0; i < columnsDisplayed.length; i++)
        {
            columnDescriptor = (JTableColumnDescriptor)
            _columnDescriptorMap.get(columnsDisplayed[i]);
            columnDescriptor.setWidth(columnWidths[i]);
        }
    }

    protected void setAlignmentOfDisplayedColumns(String columnAlignments[])
    {
        String columnsDisplayed[] = getColumnNamesThatAreCurrentlyDisplayed();

        JTableColumnDescriptor columnDescriptor;
        for(int i=0; i < columnsDisplayed.length; i++)
        {
            columnDescriptor = (JTableColumnDescriptor)
            _columnDescriptorMap.get(columnsDisplayed[i]);
            columnDescriptor.setAlignment(columnAlignments[i]);
        }
    }

  
    // -----------------------------------------------------------------------------------------------------------------------
    protected void selectColumnForSorting(String columnName, boolean isAscending, int positionToInsert,
            boolean shouldReverseAscOrDescOrder)
    {
      //  String header = "JTableColumnDescriptorManager.selectColumnForSorting() [new]: ";

        //printColumnsToSortBy("before " + header);

        JTableColumnDescriptor columnDescription = (JTableColumnDescriptor) _columnDescriptorMap.get(columnName);

        if(shouldReverseAscOrDescOrder) //flip the sort order of this column
        {
            isAscending = columnDescription.getSortAsc(); // get prev sort direction (asc or desc) 
            columnDescription.setSortAsc( ! isAscending ); // reverse the prev asc to desc or vice versa
        }
        else
        {
            columnDescription.setSortAsc( isAscending );// set the sort direction passed as argument to this method 
        }
        
        columnDescription.setSortOrder(positionToInsert);

        //put in the correct order relative to other columns
        int index = 0;

        index = _columnDescriptorForSortInfoList.indexOf(columnDescription);

        if(index != -1) //the column  is already in the list
        {
            _columnDescriptorForSortInfoList.remove(index); // remove it
            _columnDescriptorForSortInfoList.add(0, columnDescription); //add it to the front of the list       
        }
        else //it is not already in the list 
        {
            if (_columnDescriptorForSortInfoList.size() >= _totalSortToRecord)  //if we need to dump a sort column, because we have too many
            {
                int lastIndex = _columnDescriptorForSortInfoList.size()-1;
                _columnDescriptorForSortInfoList.remove(lastIndex);
            }

            if( positionToInsert == -1) //we just want it at the front
            {
                _columnDescriptorForSortInfoList.add(0, columnDescription);
            }
            else //we want it at a specific position
            {
                _columnDescriptorForSortInfoList.add(positionToInsert, columnDescription);
            }

        } //end of else it is not already in the list

        //reset the sort order integer value for the descriptors
        for (int i = 0; i < _columnDescriptorForSortInfoList.size(); i++)
        {
            JTableColumnDescriptor desc = (JTableColumnDescriptor) _columnDescriptorForSortInfoList.get(i);
            desc.setSortOrder(i);
        }

       // printColumnsToSortBy("after " + header);

    } //end of selectColumnForSorting

    // ------------------------------------------------------------------------------------------------------

    private void printColumnsToSortBy(String message)
    {
        String header = "JTableColumnDescriptorList.printColumnstoSortBy() ";

        System.out.println(message);

        for (int i = 0; i < _columnDescriptorForSortInfoList.size() ; i++)
        {
            JTableColumnDescriptor columnDescriptor = (JTableColumnDescriptor) _columnDescriptorForSortInfoList.get(i);
            if (columnDescriptor != null)
            {
                String direction = "ASC";

                if (! columnDescriptor.getSortAsc())
                {
                    direction = "DESC";
                }
                System.out.println(i + " " + columnDescriptor.getColumnName() + 
                    " " + columnDescriptor.getSortOrder() + " " + direction);
            }
            else
            {
                System.out.println(header + " column at index " + i + " is null");
            }
        }

        System.out.println();
        
    }

    // ------------------------------------------------------------------------------------------------------
    protected String[] getColumnNamesForSortToBeDone()
    {

        String columnNamesForSorting[] = new String[_columnDescriptorForSortInfoList.size()];

        for(int i=0; i < columnNamesForSorting.length; i++)
        {
            JTableColumnDescriptor columnDescriptor = (JTableColumnDescriptor)
            _columnDescriptorForSortInfoList.get(i);
            columnNamesForSorting[i] = columnDescriptor.getColumnName();
        }
        return columnNamesForSorting;

    }   

    // ------------------------------------------------------------------------------------------------------
    protected int[] getSortOrderForSortedColumnNames()
    {

        int sortOrderForSortedColumns[] = new int[_columnDescriptorForSortInfoList.size()];

        for(int i=0; i < sortOrderForSortedColumns.length; i++)
        {
            JTableColumnDescriptor columnDescriptor = (JTableColumnDescriptor)
            _columnDescriptorForSortInfoList.get(i);
            sortOrderForSortedColumns[i] = columnDescriptor.getSortOrder();
        }
        return sortOrderForSortedColumns;

    }    

//  ------------------------------------------------------------------------------------------------------

    private String[] getColumnSortSettings()
    {
        String columnSortSetting[] = null;
        JTableColumnDescriptor columnDescriptor;

        String header = "JTableColumnDescriptorManager.getColumnSortSettings(): ";
        if  (_columnDescriptorForSortInfoList.size() > 0)
        {
            int j=0;
            columnSortSetting = new String[_columnDescriptorForSortInfoList.size()];
            _columnSettingMap = new HashMap();
            for(int i=0 ; i < _columnDescriptorList.size(); i++)
            {
                columnDescriptor = (JTableColumnDescriptor) _columnDescriptorList.get(i);

                String columnName = columnDescriptor.getColumnName();
                boolean isAscending = columnDescriptor.getSortAsc();

                int columnSortOrder = -1;
                int index = _columnDescriptorForSortInfoList.indexOf(columnDescriptor);
                columnSortOrder = index;


                if(columnSortOrder != -1)  //if this column is to be sorted by
                {
                    _columnSettingMap.put(new Integer(columnSortOrder).toString() , columnDescriptor);
                    columnSortSetting[j++] = "SORT:"+columnName+"|"+ isAscending+"|"+columnSortOrder;
                }
            }
        }
        String finalSortSetting[]= null;
        if(columnSortSetting != null)
        {
            if(columnSortSetting.length > _totalSortToRecord)
                finalSortSetting = new String[_totalSortToRecord];
            else
                finalSortSetting = new String[columnSortSetting.length];
            for(int i=0; i < finalSortSetting.length; i++)
            {
                columnDescriptor = (JTableColumnDescriptor) _columnSettingMap.get(new Integer(i).toString()); 
                finalSortSetting[i] = "SORT:"+columnDescriptor.getColumnName()+"|"+ columnDescriptor.getSortAsc()+"|"+i;
            }
        }
        return finalSortSetting;
    }

    private void setSortOrderOfColumns(boolean columnSortOrders[], String columnNamesToSort[], int columnSortOrderNumber[])
    {
        JTableColumnDescriptor columnDescriptor;
        for(int i=0; i < _columnDescriptorList.size(); i++)
        {
            columnDescriptor = (JTableColumnDescriptor) _columnDescriptorList.get(i);
            for(int j=0; j< columnNamesToSort.length ; j++)
            {
                //check if the column name is chosen for sort and then set the sort info
                if(columnDescriptor.getColumnName().compareTo(columnNamesToSort[j]) == 0)
                {
                    columnDescriptor.setSortAsc(columnSortOrders[j]);
                    columnDescriptor.setSortOrder(columnSortOrderNumber[j]);
                }
            }
        }
    }

} //end of JTableColumnDescriptor
