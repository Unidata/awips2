package org.rzo.yajsw.srvmgr.client;

import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeSupport;
import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Timer;
import java.util.TimerTask;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Executor;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.SwingUtilities;

import org.rzo.netty.ahessian.rpc.client.AsyncHessianProxy;
import org.rzo.yajsw.os.ServiceInfo;
import org.rzo.yajsw.os.ServiceInfoImpl;
import org.rzo.yajsw.srvmgr.server.ServiceManagerServer;
import org.rzo.yajsw.util.BeanTableFormat;
import org.rzo.yajsw.util.ObservableList;
import org.rzo.yajsw.util.ObservableObject;

import ca.odell.glazedlists.BasicEventList;
import ca.odell.glazedlists.EventList;
import ca.odell.glazedlists.GlazedLists;
import ca.odell.glazedlists.ObservableElementList;
import ca.odell.glazedlists.SortedList;
import ca.odell.glazedlists.gui.TableFormat;
import ca.odell.glazedlists.swing.EventSelectionModel;
import ca.odell.glazedlists.swing.EventTableModel;
import ca.odell.glazedlists.swing.TableComparatorChooser;

public class ServicesTable
{
	
    String[] propertyNames = {"host", "displayName", "account", "pid", "wrapped", "wrapperAppPid"};
    String[] columnLabels = {"Host", "Name", "Account", "Pid", "YAJSW", "App Pid"};
    final Map<String, AsyncServiceManagerServer> _managers = Collections.synchronizedMap(new HashMap<String, AsyncServiceManagerServer>());
    EventSelectionModel<ObservableObject> _selection;

	
	ServicesTable(JTable jTable)
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
	    final ObservableList<ServiceInfo> list = new ObservableList(servicesEventList, propertyNames, new String[]{"host", "name"});
	    jTable.setModel(servicesTableModel);
	    _selection = new EventSelectionModel<ObservableObject>(servicesEventList);
	    jTable.setSelectionModel(_selection);
	    TableComparatorChooser tableSorter = TableComparatorChooser.install(jTable, servicesEventList, TableComparatorChooser.SINGLE_COLUMN);
	    new Timer("services table updated", true).schedule(new TimerTask()
	    {

			@Override
			public void run()
			{
				List<ServiceInfo> allServices = new ArrayList();
				HashSet<String> managers;
				synchronized (_managers)
				{
				 managers = new HashSet<String>(_managers.keySet());
				}
								
				for (String managerName : managers)
				{
					Map<String, ServiceInfo> list = null;
					try
					{
						AsyncServiceManagerServer manager = _managers.get(managerName);
						if (manager == null)
							continue;
						list = (Map<String, ServiceInfo>) ((Future)manager.getServiceList()).get(10, TimeUnit.SECONDS);
						for (ServiceInfo info : list.values())
						{
							((ServiceInfoImpl)info).setHost(managerName);
						}
					}
					catch (Exception e)
					{
						e.printStackTrace();
					}
					if (list == null || list.isEmpty())
					{
						System.out.println("error getting services");
					}
					else
					{
						for (Object service : list.values())
							if (!ClientMain.hidden.containsObject((ServiceInfo) service))
							allServices.add((ServiceInfo) service);
					}
				}
				
				list.update(allServices);
			}
	    	
	    },
	    0,
	    500);
	}
	
	public void addService(String hostName, AsyncServiceManagerServer manager)
	{
		if (manager == null)
			return;
		
		synchronized(_managers)
		{
			if (manager == null)
				return;

			if (_managers.get(hostName) == null)
			_managers.put(hostName, manager);
		}
	}
	
	public void removeService(String managerName)
	{
			_managers.remove(managerName);
	}
	
	public List<ServiceInfo> getSelection()
	{
		List<ServiceInfo> result = new ArrayList<ServiceInfo>();
		EventList<ObservableObject> selection = _selection.getSelected();
		for (ObservableObject x : selection)
			result.add((ServiceInfo) x.getRoot());
		return result;
	}
	

}
