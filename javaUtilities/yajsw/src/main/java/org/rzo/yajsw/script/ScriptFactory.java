/* This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * <p/>
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.  
 */
package org.rzo.yajsw.script;

import java.util.List;

import org.jboss.netty.logging.InternalLogger;
import org.rzo.yajsw.wrapper.WrappedProcess;

// TODO: Auto-generated Javadoc
/**
 * A factory for creating Script objects.
 */
public class ScriptFactory
{

	/**
	 * Creates a new Script object.
	 * 
	 * @param script
	 *            the script
	 * @param timeout
	 * 
	 * @return the script
	 */
	public static Script createScript(String script, String id, WrappedProcess process, String[] args, InternalLogger log, int timeout, String encoding, boolean reload, boolean debug)
	{
		if (script == null || "".equals(script))
			return null;
		if (log != null && debug)
			log.info("create script: " + script);
		if (script.endsWith(".bat") || script.endsWith(".sh"))
			return new ShellScript(script, id, process, args, timeout);
		if (script.endsWith(".gv") || script.endsWith(".groovy"))
			try
			{
				return new GroovyScript(script, id, process, args, timeout, log, encoding, reload);
			}
			catch (Throwable e)
			{
				if (log != null)
					log.info("Error in createScript " + script, e);
			}
		return null;
	}

	public static Script createScript(String script, String id, WrappedProcess process, List args, InternalLogger log, int timeout, String encoding, boolean reload, boolean debug)
	{
		String[] argsArr = new String[0];
		if (args != null && args.size() > 0)
		{
			argsArr = new String[args.size()];
			for (int i = 0; i < argsArr.length; i++)
				argsArr[i] = args.get(i).toString();
		}
		return createScript(script, id, process, argsArr, log, timeout, encoding, reload, debug);
	}

}
