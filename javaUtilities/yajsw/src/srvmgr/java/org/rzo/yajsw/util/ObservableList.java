package org.rzo.yajsw.util;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.apache.commons.beanutils.PropertyUtils;
import org.rzo.yajsw.srvmgr.client.Host;

import ca.odell.glazedlists.EventList;

public class ObservableList<E>
{
	EventList<ObservableObject>		_list;
	String[]						_idProperty;
	String[]						_propertyNames;

	Map<Object, ObservableObject>	_currentList	= new HashMap<Object, ObservableObject>();

	public ObservableList(EventList<ObservableObject> list, String[] propertyNames, String[] idProperty)
	{
		_list = list;
		_idProperty = idProperty;
		_propertyNames = propertyNames;
	}

	private String getId(Object obj)
	{
		String result = "";
		try
		{
			for (String prop : _idProperty)
				result += PropertyUtils.getSimpleProperty(obj, prop) + "$$";
			return result;
		}
		catch (Exception e)
		{
			e.printStackTrace();
		}
		return null;
	}

	public void update(Collection<E> newList)
	{
		Set<Object> updated = new HashSet<Object>();
		for (Object obj : newList)
		{
			updated.add(updateObject(obj));
		}
		Set<Object> toRemove = new HashSet<Object>(_currentList.keySet());
		toRemove.removeAll(updated);
		for (Object id : toRemove)
		{
			_list.remove(_currentList.remove(id));
		}

	}

	public Object updateObject(Object obj)
	{
		Object id = getId(obj);
		ObservableObject current = _currentList.get(id);
		if (current == null)
		{
			current = new ObservableObject(obj, _propertyNames);
			_currentList.put(id, current);
			_list.add(current);
		}
		else
			current.update(obj);
		return id;
	}
	
	public void removeObject(E obj)
	{
		Object id = getId(obj);
		ObservableObject current = _currentList.remove(id);
		if (current != null)
			_list.remove(current);

	}
	
	public boolean containsObject(E obj)
	{
		return _currentList.containsKey(getId(obj));
	}

	public E getObject(E obj)
	{
		Object id = getId(obj);
		ObservableObject current = _currentList.get(id);
		if (current == null)
			return null;
		return (E) current.getRoot();
	}
	
	public Collection<E> getObjectList()
	{
		List<E> result = new ArrayList<E>();
		for (ObservableObject obj : _currentList.values())
		{
			result.add((E) obj.getRoot());
		}
		return result;
	}
	

}
