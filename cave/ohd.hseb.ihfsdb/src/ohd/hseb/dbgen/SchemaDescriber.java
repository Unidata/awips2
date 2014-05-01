/*
 * Created on Sep 16, 2004
 *
 * 
 */
package ohd.hseb.dbgen;


import java.sql.DatabaseMetaData;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Types;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import ohd.hseb.db.Database;
import ohd.hseb.db.DbType;

/**
 * @author GobsC
 *
 * This class encapsulates the discovery of the database schema for code generation purposes.
 */
public class SchemaDescriber
{

    private Database _db; 
    private DbType _dbType = null;
    
    
    // ---------------------------------------------------------------------------------
    
    public SchemaDescriber(Database database)
    {
        _db = database;
        _dbType = _db.getDbType();
    }
    //-------------------------------------------------------------------------------------
  
    public List getTableDescriptorList(Map preferredTableNameMap)
    {
        List descriptorList = null;


        if (_dbType == DbType.Informix)
        {
            descriptorList = getInformixTableDescriptorList(preferredTableNameMap);
        }
        else  //use the PostgreSQL way, which we assume to be more standard
        {
            descriptorList = getPostgreSQLTableDescriptorList(preferredTableNameMap);
        }

        return descriptorList;

    }
 
    
    //-------------------------------------------------------------------------------------
    
    
    public List getPostgreSQLTableDescriptorList(Map preferredTableNameMap)
    {
        String header = "getPostgreSQLTableDescriptorList(): ";
        List tdList = new ArrayList();
      
        try
        {

            // Database Meta Data object
            DatabaseMetaData dbmd = _db.getConnection().getMetaData();


            //get all the info on regular tables

            boolean isView = false;

            // get the basic info on all the true tables
            // added capitalized "TABLE" for postgresql
            String[] tableTypesStringArray = { "table", "TABLE" };
            isView = false;
            addToRawTableDescriptorList(dbmd, preferredTableNameMap, isView,
                    tableTypesStringArray, tdList);

            // get the basic info on all the views
            String[] tableTypesStringArray2 = { "view", "VIEW" };
            // added capitalized "VIEW" for postgresql
            isView = true;
            addToRawTableDescriptorList(dbmd, preferredTableNameMap, isView,
                    tableTypesStringArray2, tdList);



            // create column descriptors for each table or view
            for (int i = 0; i < tdList.size(); i++)
            {
                TableDescriptor tableDesc = (TableDescriptor) tdList.get(i);
                String origCaseTableName = tableDesc.getOriginalCaseTableName();

                // get the column info for all tables and views
                //  ResultSet rsForColumn = dbmd.getColumns("", "", tableName,
                // "%");
                ResultSet rsForColumn = dbmd.getColumns(null, null,
                        origCaseTableName, "%");

                // get the primary keys for this particular table
                // and store them in a set
                //  ResultSet rsForKeys = dbmd.getPrimaryKeys("", "", tableName);
                ResultSet rsForKeys = dbmd.getPrimaryKeys(null, null,
                        origCaseTableName);

                Set keySet = new HashSet();
                while (rsForKeys.next())
                {
                    String keyColumnName = rsForKeys.getString(4);
                    keySet.add(keyColumnName.toLowerCase());
                }

                if (keySet.size() > 0)
                {
                    tableDesc.setHasPrimaryKey(true);
                }
                else
                {
                    tableDesc.setHasPrimaryKey(false);
                }


                // for each column, make a corresponding ColumnDescriptor and
                // add it to the list contained by the TableDescriptor
                while (rsForColumn.next())
                {
                    String columnName = rsForColumn.getString(4).trim();

                    int sqlTypeInt = rsForColumn.getInt(5);
                    String sqlTypeString = rsForColumn.getString(6).trim();
                    
                    boolean isNullable = false;
                    int nullableCode = rsForColumn.getInt(11);             
                    if ( (nullableCode == DatabaseMetaData.columnNullable) ||
                         (nullableCode == DatabaseMetaData.columnNullableUnknown)
                       )
                    {
                        isNullable = true;
                    }
                    else
                    {
                        isNullable = false;
                        
                        /*
                        System.out.println("DatabaseMetaData.columnNoNulls = " +  
                                           DatabaseMetaData.columnNoNulls);
                        System.out.println("************ nullablecode = " +
                                            nullableCode + " for " + columnName +
                                            " in " + tableDesc.getName());
                        */
                    }
                    
                    
                    
                    //special case for PostgreSQL text field
                    // needed because it comes out as Types.VARCHAR
                    if (sqlTypeString.equals("text"))
                    {
                        sqlTypeInt = Types.LONGVARCHAR;
                    }
                    
                    int colSize = rsForColumn.getInt(7);
           
                
                    ColumnDescriptor colDesc = new ColumnDescriptor();
                    colDesc.setName(columnName);
                    colDesc.setSqlTypeInt(sqlTypeInt);
                    colDesc.setSqlTypeString(sqlTypeString);
                    colDesc.setSize(colSize);
                    colDesc.setNullable(isNullable);
           
                    // set the KeyColumn boolean field as appropriate
                    if (keySet.contains(columnName.toLowerCase()))
                    {
                        colDesc.setKeyColumn(true);
                    }
                    else
                    {
                        colDesc.setKeyColumn(false);
                    }

                    // add the columnDescriptor to the tableDescriptor's list
                    tableDesc.getColumnDescriptorList().add(colDesc);

                      
                } //end while

                //System.out.println(header + "tableDesc = " + tableDesc);
                

            } //end for



        }
        catch (SQLException e)
        {
            System.out.println(this.getClass().getName()
                    + ".getTableDescriptorList():" + " Problem with SQL "
                    + e.getMessage());
            e.printStackTrace();
        }
        return tdList;

    } //end getPostgreSQLTableDescriptorList

    //-------------------------------------------------------------------------------------
    public List getInformixTableDescriptorList(Map preferredTableNameMap)
    {
        List tdList = new ArrayList();
        System.out.println("getInformixTableDescriptorList(): begin");
        try
        {

            // Database Meta Data object
            DatabaseMetaData dbmd = _db.getConnection().getMetaData();


            //get all the info on regular tables

            boolean isView = false;

            // get the basic info on all the true tables
            String[] tableTypesStringArray = { "table"};
            isView = false;
            addToRawTableDescriptorList(dbmd, preferredTableNameMap, isView,
                    tableTypesStringArray, tdList);

            // get the basic info on all the views
            String[] tableTypesStringArray2 = { "view"};
            isView = true;
            addToRawTableDescriptorList(dbmd, preferredTableNameMap, isView,
                    tableTypesStringArray2, tdList);



            // create column descriptors for each table or view
            for (int i = 0; i < tdList.size(); i++)
            {
                TableDescriptor tableDesc = (TableDescriptor) tdList.get(i);
                String origCaseTableName = tableDesc.getOriginalCaseTableName();

                // get the column info for all tables and views
                ResultSet rsForColumn = dbmd.getColumns("", "", origCaseTableName, "%");
                
                // get the primary keys for this particular table
                // and store them in a set
                ResultSet rsForKeys = dbmd.getPrimaryKeys("", "", origCaseTableName);
                

                Set keySet = new HashSet();
                while (rsForKeys.next())
                {
                    String keyColumnName = rsForKeys.getString(4);
                    keySet.add(keyColumnName.toLowerCase());
                }

                if (keySet.size() > 0)
                {
                    tableDesc.setHasPrimaryKey(true);
                }
                else
                {
                    tableDesc.setHasPrimaryKey(false);
                }



                // for each column, make a corresponding ColumnDescriptor and
                // add it to the list contained by the TableDescriptor
                while (rsForColumn.next())
                {
                    String columnName = rsForColumn.getString(4).trim();

                    String sqlTypeString = rsForColumn.getString(6).trim();
                    
                    int sqlTypeInt = rsForColumn.getInt(5);
               
                    ColumnDescriptor colDesc = new ColumnDescriptor();
                    colDesc.setName(columnName);
                    colDesc.setSqlTypeInt(sqlTypeInt);
                    colDesc.setSqlTypeString(sqlTypeString);
              

                    // set the KeyColumn boolean field as appropriate
                    if (keySet.contains(columnName.toLowerCase()))
                    {
                        colDesc.setKeyColumn(true);
                    }
                    else
                    {
                        colDesc.setKeyColumn(false);
                    }


                    // add the columnDescriptor to the tableDescriptor's list
                    tableDesc.getColumnDescriptorList().add(colDesc);

                } //end while



            } //end for

        }
        catch (SQLException e)
        {
            System.out.println(this.getClass().getName()
                    + ".getTableDescriptorList():" + " Problem with SQL "
                    + e.getMessage());
            e.printStackTrace();
        }
        return tdList;

    } //getInformixTableDescriptorList
 
    //-------------------------------------------------------------------------------------
    
    
    // sub method of getTableDescriptorList
    // broken out to reduce duplicate code for tables and views
    private void addToRawTableDescriptorList(DatabaseMetaData dbmd,
                                             Map preferredTableNameMap,
                                             boolean isView,
                                             String[] tableTypeNameArray,
                                             List rawTableDescriptorList)
            throws SQLException
    {
        //ResultSet rsForTables = dbmd.getTables("", "", "%",
        // tableTypeNameArray );
        ResultSet rsForTables = dbmd.getTables(null, null, "%",
                tableTypeNameArray);



        while (rsForTables.next())
        {
            TableDescriptor tableDesc = new TableDescriptor();

            String originalCaseName = rsForTables.getString(3);
            String lowerCaseName = originalCaseName.toLowerCase();
            String mixedCaseName = (String) preferredTableNameMap.get(lowerCaseName);
            if (mixedCaseName == null)
            {
                mixedCaseName = lowerCaseName;
            }

            tableDesc.setOriginalCaseTableName(originalCaseName);
            tableDesc.setName(mixedCaseName);
            
            /*
            System.out.println("addToRawTableDescriptorList(): original case of table name = " +
                            			originalCaseName + 
                            			" mixed case table name = "  +
                            			mixedCaseName);
            */
            
            tableDesc.setView(isView);
            rawTableDescriptorList.add(tableDesc);
        }

        rsForTables.close();

        return;
    }


  
  
   
    
} //end SchemaDescriber
