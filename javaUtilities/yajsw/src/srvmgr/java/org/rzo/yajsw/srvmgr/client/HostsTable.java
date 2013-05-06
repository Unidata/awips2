package org.rzo.yajsw.srvmgr.client;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Timer;
import java.util.TimerTask;

import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;

import org.rzo.yajsw.os.ServiceInfo;
import org.rzo.yajsw.srvmgr.server.ServiceManagerServer;
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

public class HostsTable  
{
    String[] propertyNames = {"name", "state", "included"};
    String[] columnLabels = {"Name", "State", "In"};
    
    ObservableList<Host> list;
    EventSelectionModel<ObservableObject> _selection;

	
    HostsTable(JTable jTable)
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
	    //JTable jTable = new JTable(servicesTableModel);
	    jTable.setModel(servicesTableModel);
	    _selection = new EventSelectionModel<ObservableObject>(servicesEventList);
	    jTable.setSelectionModel(_selection);
	    TableComparatorChooser tableSorter = TableComparatorChooser.install(jTable, servicesEventList, TableComparatorChooser.SINGLE_COLUMN);
	    Dimension size = jTable.getPreferredScrollableViewportSize();
//	    jTable.setPreferredScrollableViewportSize
//	        (new Dimension(jTable.getPreferredSize().width, size.height));
//	    JScrollPane servicesListScrollPane = new JScrollPane(jTable);
//	    this.setLayout(new BorderLayout());
//	    this.add(servicesListScrollPane, BorderLayout.CENTER);
	}
    
    public void updateObject(Host host)
    {
    	list.updateObject(host);
    }

    public void removeObject(Host host)
    {
    	list.removeObject(host);
    }
    
	public List<Host> getSelection()
	{
		List<Host> result = new ArrayList<Host>();
		EventList<ObservableObject> selection = _selection.getSelected();
		for (ObservableObject x : selection)
			result.add((Host) x.getRoot());
		return result;
	}
	
	public Host getObject(Host host)
	{
		return list.getObject(host);
	}
	
	public Collection<Host> getObjectList()
	{
		return list.getObjectList();
	}
	


}
