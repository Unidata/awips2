package org.rzo.yajsw.util;

import java.util.HashMap;
import java.util.Map;

import org.apache.commons.beanutils.PropertyUtils;

import ca.odell.glazedlists.gui.TableFormat;

public class BeanTableFormat<E>  implements TableFormat<E>
{
	String[] _columnNames;
	Map<Integer, String> _propertyNames = new HashMap<Integer, String>();
	
	public BeanTableFormat(String[] propertyNames, String[] columnNames)
	{
		_columnNames = columnNames;
		for (int i=0; i<propertyNames.length; i++)
			_propertyNames.put(i, propertyNames[i]);
	}

	public int getColumnCount()
	{
		return _columnNames.length;
	}

	public String getColumnName(int i)
	{
		return _columnNames[i];
	}

	public Object getColumnValue(E obj, int i)
	{
		try
		{
			if (obj instanceof ObservableObject)
				return PropertyUtils.getSimpleProperty(((ObservableObject)obj)._root, _propertyNames.get(i));
			else
				PropertyUtils.getSimpleProperty(obj, _propertyNames.get(i));
		}
		catch (Exception e)
		{
			e.printStackTrace();
		}
		return null;
	}

}
