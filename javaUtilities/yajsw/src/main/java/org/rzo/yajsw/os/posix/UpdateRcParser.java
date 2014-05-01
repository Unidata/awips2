package org.rzo.yajsw.os.posix;

import java.util.HashSet;
import java.util.Set;

public class UpdateRcParser
{

	Set<String>	_startLinks		= new HashSet<String>();
	Set<String>	_stopLinks		= new HashSet<String>();
	String		_runLevelDir;
	String		_serviceName;
	String		_stopLevels		= "";
	String		_startLevels	= "";

	public UpdateRcParser(String property, String runLevelDir, String serviceName)
	{
		if (runLevelDir == null || "".equals(runLevelDir))
			return;
		if (serviceName == null || "".equals(serviceName))
			return;

		_runLevelDir = runLevelDir;
		_serviceName = serviceName;

		if (property == null || "".equals(property))
			setDefault();
		else
			setLinks(property);

	}

	private void setDefault()
	{
		setDefaultStartLinks(20);
		setDefaultStopLinks(20);
	}

	private void setDefaultStopLinks(int priority)
	{
		addStopLink(0, priority);
		addStopLink(1, priority);
		addStopLink(6, priority);
	}

	private void addStopLink(String level, int priority)
	{
		_stopLinks.add(_runLevelDir.replaceFirst("X", "" + level) + "/K" + priority + _serviceName);
	}

	private void addStopLink(int level, int priority)
	{
		_stopLinks.add(_runLevelDir.replaceFirst("X", "" + level) + "/K" + priority + _serviceName);
	}

	private void setDefaultStartLinks(int priority)
	{
		addStartLink(2, priority);
		addStartLink(3, priority);
		addStartLink(4, priority);
		addStartLink(5, priority);
	}

	private void addStartLink(String level, int priority)
	{
		_startLinks.add(_runLevelDir.replaceFirst("X", "" + level) + "/S" + priority + _serviceName);
	}

	private void addStartLink(int level, int priority)
	{
		_startLinks.add(_runLevelDir.replaceFirst("X", "" + level) + "/S" + priority + _serviceName);
	}

	private void setLinks(String property)
	{
		try
		{
			String[] x = property.split(" ");
			if (x.length == 1)
			{
				int priority = Integer.parseInt(x[0]);
				setDefaultStartLinks(priority);
				setDefaultStopLinks(priority);
			}
			else if (x.length == 2)
			{
				int startPriority = Integer.parseInt(x[0]);
				int stopPriority = Integer.parseInt(x[1]);
				setDefaultStartLinks(startPriority);
				setDefaultStopLinks(stopPriority);
			}
			else
			{
				int i = 0;
				int priority=0;
				String level="0";
				String txt;

        while ( i < x.length )
        {
          txt = x[i++];
          if ( txt.trim().equals( "start" ) )
          {
            priority = Integer.parseInt( x[i++] );
            while ( i < x.length && !".".equals( x[i].trim() ) )
            {
              txt = x[i++].trim();

              // Allow S for Single User Mode ( Solaris )
              if ( txt.equalsIgnoreCase( "S" ) )
                level = "S";
              else
                level = "" + Integer.parseInt( txt );

              addStartLink( level, priority );
              _startLevels += level + " ";
            }
          }
          else if ( txt.trim().equals( "stop" ) )
          {
            priority = Integer.parseInt( x[i++] );
            while ( i < x.length && !".".equals( x[i].trim() ) )
            {
              txt = x[i++].trim();

              // Allow S for Single User Mode ( Solaris )
              if ( txt.equalsIgnoreCase( "S" ) )
                level = "S";
              else
                level = "" + Integer.parseInt( txt );

              addStopLink( level, priority );
              _stopLevels += level + " ";

						}
					}
				}
			}
		}
		catch (Exception ex)
		{
			System.out.println("error parsing wrapper.daemon.update_rc " + ex);
		}
	}

	public Set<String> getStopLinks()
	{
		return _stopLinks;
	}

	public Set<String> getStartLinks()
	{
		return _startLinks;
	}

	public static void main(String[] args)
	{
		UpdateRcParser p;

		p = new UpdateRcParser(null, "/etc/rcX.d", "serviceName");
		System.out.println(p.getStartLinks());
		System.out.println(p.getStopLinks());

		p = new UpdateRcParser("91", "/etc/rcX.d", "serviceName");
		System.out.println(p.getStartLinks());
		System.out.println(p.getStopLinks());

		p = new UpdateRcParser("20 80", "/etc/rcX.d", "serviceName");
		System.out.println(p.getStartLinks());
		System.out.println(p.getStopLinks());

		p = new UpdateRcParser("start 20 2 3 4 . start 30 5 . stop 80 0 1 6", "/etc/rcX.d", "serviceName");
		System.out.println(p.getStartLinks());
		System.out.println(p.getStopLinks());

	}

	public String getStopLevels()
	{
		return _stopLevels;
	}

	public String getStartLevels()
	{
		return _startLevels;
	}

}
