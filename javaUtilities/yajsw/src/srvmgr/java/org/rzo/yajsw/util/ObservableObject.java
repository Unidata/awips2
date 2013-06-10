package org.rzo.yajsw.util;

import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeSupport;
import java.lang.reflect.InvocationTargetException;

import org.apache.commons.beanutils.PropertyUtils;
import org.rzo.yajsw.os.ServiceInfo;

public class ObservableObject implements Comparable<ObservableObject>
{
	Object _root;
	String[] _propertyNames;
	String _idName;
	
	public ObservableObject(Object obj, String[] propertyNames)
	{
		_root = obj;
		_propertyNames = propertyNames;
	}
	
    private final PropertyChangeSupport support = new PropertyChangeSupport(this);
    public void addPropertyChangeListener(PropertyChangeListener l) {
        support.addPropertyChangeListener(l);
    }

    public void removePropertyChangeListener(PropertyChangeListener l) {
        support.removePropertyChangeListener(l);
    }
    
    public void update(Object obj)
    {
    	for (String field : _propertyNames)
    		update(field, obj);
    }
    
    public void update(String field, Object obj)
    {
    	Object newValue;
		try
		{
			newValue = PropertyUtils.getSimpleProperty(obj, field);
    	Object oldValue = PropertyUtils.getSimpleProperty(_root, field);
    	if (oldValue != null && !oldValue.equals(newValue))
    	{
    		PropertyUtils.setSimpleProperty(_root, field, newValue);
    		support.firePropertyChange(field, oldValue, newValue);
    	}
		}
		catch (Exception e)
		{
			e.printStackTrace();
		}
    }

	public int compareTo(ObservableObject o)
	{
		Comparable t1 = (Comparable) _root;
		Comparable t2 = (Comparable) o._root;
		return t1.compareTo(t2);
	}
	
	public Object getRoot()
	{
		return _root;
	}

}
