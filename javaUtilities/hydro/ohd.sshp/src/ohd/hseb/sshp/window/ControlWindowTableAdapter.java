/*
 * Created on Aug 19, 2004
 *
 *
 */
package ohd.hseb.sshp.window;

import java.util.List;

import ohd.hseb.model.LocationDescriptor;
import ohd.hseb.sshp.gui.TableAdapter;
import ohd.hseb.util.gui.TableColumnComparator;


/**
 * @author GobsC
 *
 * 
 */
public class ControlWindowTableAdapter extends TableAdapter
{
    
    private LocationDescriptor _descriptor = null;
    private List _descriptorList = null;
    
    // ---------------------------------------------------------------------------------
     
    
    
    public ControlWindowTableAdapter(TableColumnComparator comparator,
                                            String[] columnNameArray,
                                            List dataList)
    {
        super(comparator, columnNameArray, dataList);
        
        setAllowMultipleSelection(false);
          
    }
    
    // ---------------------------------------------------------------------------------
    public String[][] getDataStringArrayFromDataList(List locationDescriptorList)
    {
    
        //set alias for cut-and-paste-ability of code
        List dataList = locationDescriptorList;
        
        int fieldCount = 6;
        String[][] dataArray = new String[dataList.size()][fieldCount];
        
        
        for (int i = 0; i < dataList.size(); i++)
        {
            LocationDescriptor desc = (LocationDescriptor) dataList.get(i);
           
            dataArray[i][0] = desc.getId();
            dataArray[i][1] = desc.getLocationName();
            
            dataArray[i][2] = desc.getBasinId();
            dataArray[i][3] = desc.getStreamName();
            dataArray[i][4] = desc.getHsa();
            dataArray[i][5] = desc.getModelPreference();
        }
        
        return dataArray;
        
    }
    // ---------------------------------------------------------------------------------
    public void refreshTableModel(List dataList)
    {
        setDataList(dataList);// for super class
        
        _descriptorList = dataList;
      
        String[][] dataStringArray = getDataStringArrayFromDataList(dataList);
    	getTableModel().updateData( dataStringArray );
    	getTableModel().fireTableChanged( null );
  

        return;
        
    }
    // ---------------------------------------------------------------------------------

    public int getDescriptorCount()
    {
        int count =  getDataList().size();
        return count;
    }
    
    // ---------------------------------------------------------------------------------

    public LocationDescriptor getDescriptorByIndex(int index)
    {
        List dataList = getDataList();
        LocationDescriptor desc = null;
        
        if ( dataList.size() > 0 )
        {
            Object object = dataList.get(index);
            desc = (LocationDescriptor) object;
        }
        return desc;
    }
    
    // ---------------------------------------------------------------------------------

    private int findIndexOfDescriptor(LocationDescriptor desc)
    {
        int index = _descriptorList.indexOf(desc);
        
        return index;
    }
    // ------------------------------------------------------------------------------------- 
     public void setDataList(List dataList)
    {
     
        super.setDataList(dataList);
        _descriptorList = dataList;
        
    }
  
     // ---------------------------------------------------------------------------------
     
    
    private void selectAll()
    {
        
        getListSelectionModel().setSelectionInterval(0, getTable().getModel().getRowCount()-1);
    }
    
    
    // ---------------------------------------------------------------------------------
    public void setSelectedIndex(int index)
    {
         
        getListSelectionModel().setSelectionInterval(index, index);
      
    }
    // ---------------------------------------------------------------------------------
    
    public void selectItems(List selectedDescriptorList)
    {
        getListSelectionModel().clearSelection();
        
        for (int i = 0; i < selectedDescriptorList.size(); i++)
        {
            LocationDescriptor descriptor = (LocationDescriptor) selectedDescriptorList.get(i);
           
            int index = findIndexOfDescriptor(descriptor);
            
            if (index >= 0)
            {
                getListSelectionModel().setSelectionInterval(index, index);     
            }
        }
    }
    // ---------------------------------------------------------------------------------
    
}
