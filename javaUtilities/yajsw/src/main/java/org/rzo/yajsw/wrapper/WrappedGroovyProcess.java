package org.rzo.yajsw.wrapper;

public class WrappedGroovyProcess extends WrappedJavaProcess
{

	protected String getMainClass()
	{
		return _config.getString("wrapper.java.mainclass", "org.rzo.yajsw.app.WrapperGroovyMain");
	}

	public String getType()
	{
		return "Groovy-" + super.getType();
	}

}
