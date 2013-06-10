package org.rzo.yajsw.srvmgr.client;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;

import org.rzo.yajsw.os.ServiceInfo;
import org.rzo.yajsw.util.BeanTableFormat;
import org.rzo.yajsw.util.ObservableList;
import org.rzo.yajsw.util.ObservableObject;

import ca.odell.glazedlists.BasicEventList;
import ca.odell.glazedlists.EventList;
import ca.odell.glazedlists.GlazedLists;
import ca.odell.glazedlists.ObservableElementList;
import ca.odell.glazedlists.SortedList;
import ca.odell.glazedlists.swing.EventSelectionModel;
import ca.odell.glazedlists.swing.EventTableModel;
import ca.odell.glazedlists.swing.TableComparatorChooser;

public class HiddenTable
{
    String[] propertyNames = {"displayName"};
    String[] columnLabels = {"Name"};
    
    ObservableList<ServiceInfo> list;
    EventSelectionModel<ObservableObject> _selection;

	
    HiddenTable(JTable jTable)
	{
        ObservableElementList.Connector<ObservableObject> connector = GlazedLists.beanConnector(ObservableObject.class);
	    SortedList<ObservableObject> servicesEventList = 
	    	    new SortedList<ObservableObject>(
	    	    new ObservableElementList<ObservableObject> (
	    	    		GlazedLists.threadSafeList(
	    	    				new BasicEventList<ObservableObject>()
	    	    				), connector
	    	    				));
	    EventTableModel<ObservableObject> servicesTableModel = new EventTableModel<ObservableObject>(servicesEventList, 
	    		new BeanTableFormat<ObservableObject>(propertyNames, columnLabels));
	    list = new ObservableList(servicesEventList, propertyNames, new String[]{"name"});
	    jTable.setModel(servicesTableModel);
	    _selection = new EventSelectionModel<ObservableObject>(servicesEventList);
	    jTable.setSelectionModel(_selection);
	}
    
    public void updateObject(ServiceInfo service)
    {
    	list.updateObject(service);
    }

    public void removeObject(ServiceInfo service)
    {
    	list.removeObject(service);
    }
    
    public boolean containsObject(ServiceInfo service)
    {
    	return list.containsObject(service);
    }
    
	public List<ServiceInfo> getSelection()
	{
		List<ServiceInfo> result = new ArrayList<ServiceInfo>();
		EventList<ObservableObject> selection = _selection.getSelected();
		for (ObservableObject x : selection)
			result.add((ServiceInfo) x.getRoot());
		return result;
	}

	public Collection<ServiceInfo> getObjectList()
	{
		return list.getObjectList();
	}
	

}
