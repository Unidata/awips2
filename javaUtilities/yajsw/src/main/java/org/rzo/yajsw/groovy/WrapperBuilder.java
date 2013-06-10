package org.rzo.yajsw.groovy;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.HashMap;
import java.util.Map;

import org.rzo.yajsw.boot.WrapperLoader;

public class WrapperBuilder extends HashMap // extends XHashMap
{
	static ClassLoader	_wrapperClassLoader	= WrapperLoader.getWrapperClassLoader();

	public Object process() throws ClassNotFoundException, NoSuchMethodException, IllegalArgumentException, IllegalAccessException,
			InvocationTargetException
	{
		Thread.currentThread().setContextClassLoader(_wrapperClassLoader);
		Class cls = _wrapperClassLoader.loadClass("org.rzo.yajsw.wrapper.WrappedProcessFactory");

		Method create = cls.getDeclaredMethod("createProcess", Map.class);
		return create.invoke(null, this);
	}

	public Object service() throws ClassNotFoundException, NoSuchMethodException, IllegalArgumentException, IllegalAccessException,
			InvocationTargetException
	{
		Thread.currentThread().setContextClassLoader(_wrapperClassLoader);
		Class cls = _wrapperClassLoader.loadClass("org.rzo.yajsw.wrapper.WrappedServiceFactory");
		Method create = cls.getDeclaredMethod("createService", Map.class);
		return create.invoke(null, this);
	}

}
