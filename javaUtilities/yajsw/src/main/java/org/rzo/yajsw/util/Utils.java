package org.rzo.yajsw.util;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;

import org.rzo.yajsw.wrapper.WrappedProcess;
import org.rzo.yajsw.wrapper.WrappedService;

public class Utils
{
	WrappedProcess	_process;
	WrappedService	_service;

	public Utils(WrappedProcess process)
	{
		_process = process;
	}

	public Utils(WrappedService service)
	{
		_service = service;
	}

	public String inquireCLI(String message) throws IOException
	{
		System.out.print(message + ":");
		return new BufferedReader(new InputStreamReader(System.in)).readLine();
	}

	public String inquireTrayIcon(String message) throws InterruptedException
	{
		String result = null;
		if (_process == null)
		{
			System.out.println("ERROR in inquireTrayIcon: process == null");
			return null;
		}
		while (result == null)
		{
			result = _process.getTrayIcon().inquire(message);
			if (result == null)
				Thread.sleep(2000);
		}
		return result;
	}


}
