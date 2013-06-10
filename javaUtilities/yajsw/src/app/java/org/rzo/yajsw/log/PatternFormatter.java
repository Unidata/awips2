/**
 *    	Copyright [2007] [Rohit B. Rai]
 *
 *     	Licensed under the Apache License, Version 2.0 (the "License");
 *     	you may not use this file except in compliance with the License.
 *     	You may obtain a copy of the License at
 *
 *	http://www.apache.org/licenses/LICENSE-2.0
 *
 *	Unless required by applicable law or agreed to in writing, software
 *	distributed under the License is distributed on an "AS IS" BASIS,
 *	WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *	See the License for the specific language governing permissions and
 *	limitations under the License.
 */
package org.rzo.yajsw.log;

import java.text.DateFormat;
import java.text.MessageFormat;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.logging.Formatter;
import java.util.logging.LogManager;
import java.util.logging.LogRecord;

// TODO: Auto-generated Javadoc
/**
 * The Class PatternFormatter.
 */
public class PatternFormatter extends Formatter
{

	/** The sh. */
	java.util.logging.SimpleFormatter	sh;

	/**
	 * <pre>
	 * The Log Formatter will use the following formatting tokens  LoggerName %LOGGER% Level %LEVEL% Time %TIME% Message %MESSAGE% SourceClassName %SOURCECLASS% SourceMethodName %SOURCEMETHOD% Exception Message %EXCEPTION% ExceptionStackTrace %STACKTRACE% Parameter %PARAM%
	 * </pre>
	 * 
	 * The default log format is "[%LOGGER% - %LEVEL%] %TIME%: %MESSAGE%" And
	 * exception format is [%LOGGER% - %LEVEL%] %TIME% %MESSAGE% \n Exception:
	 * %EXCEPTION% \n %STACKTRACE% Apart from this the time format may be
	 * specified in satand java time format in the timeFormat variable The
	 * default time format is "dd-MMM-yyy; HH:mm:ss".
	 */

	private String						logPattern;

	/** The exception pattern. */
	private String						exceptionPattern;

	/** The time format. */
	private String						timeFormat;

	/** The log message format. */
	private MessageFormat				logMessageFormat;

	/** The exception message format. */
	private MessageFormat				exceptionMessageFormat;

	/** The date format. */
	private DateFormat					dateFormat;

	/**
	 * Instantiates a new pattern formatter.
	 */
	public PatternFormatter()
	{
		LogManager manager = LogManager.getLogManager();
		String cname = getClass().getName();

		timeFormat = manager.getProperty(cname + ".timeFormat");

		if (timeFormat == null)
		{
			timeFormat = "dd-MMM-yyy; HH:mm:ss";
		}
		setTimeFormat(timeFormat);

		logPattern = manager.getProperty(cname + ".logPattern");
		if (logPattern == null)
		{
			logPattern = "[{0} - {1}] {2}: {3} \n";
		}
		setLogPattern(logPattern);

		exceptionPattern = manager.getProperty(cname + ".exceptionPattern");
		if (exceptionPattern == null)
		{
			exceptionPattern = "[{0} - {1}] {2} {3} \nException in {4}: {6} \n{7} ";
		}
		setExceptionPattern(exceptionPattern);

		logMessageFormat = new MessageFormat(logPattern);
		exceptionMessageFormat = new MessageFormat(exceptionPattern);

		dateFormat = new SimpleDateFormat(timeFormat);
	}

	/**
	 * Sets the time format.
	 * 
	 * @param timeFormat
	 *            the new time format
	 */
	public void setTimeFormat(String timeFormat)
	{
		this.timeFormat = timeFormat;
		dateFormat = new SimpleDateFormat(timeFormat);
	}

	/**
	 * Sets the log pattern.
	 * 
	 * @param logFormat
	 *            the new log pattern
	 */
	public void setLogPattern(String logFormat)
	{
		logFormat = logFormat.replace("%LOGGER%", "{0}");
		logFormat = logFormat.replace("%LEVEL%", "{1}");
		logFormat = logFormat.replace("%TIME%", "{2}");
		logFormat = logFormat.replace("%MESSAGE%", "{3}");
		logFormat = logFormat.replace("%SOURCECLASS%", "{4}");
		logFormat = logFormat.replace("%SOURCEMETHOD%", "{5}");
		logFormat = logFormat.replace("%PARAM0%", "{6}");
		logFormat = logFormat.replace("%PARAM1%", "{7}");

		this.logPattern = logFormat;

		logMessageFormat = new MessageFormat(logPattern);
	}

	/**
	 * Sets the exception pattern.
	 * 
	 * @param exceptionFormat
	 *            the new exception pattern
	 */
	public void setExceptionPattern(String exceptionFormat)
	{
		exceptionFormat = exceptionFormat.replace("%LOGGER%", "{0}");
		exceptionFormat = exceptionFormat.replace("%LEVEL%", "{1}");
		exceptionFormat = exceptionFormat.replace("%TIME%", "{2}");
		exceptionFormat = exceptionFormat.replace("%MESSAGE%", "{3}");
		exceptionFormat = exceptionFormat.replace("%SOURCECLASS%", "{4}");
		exceptionFormat = exceptionFormat.replace("%SOURCEMETHOD%", "{5}");
		exceptionFormat = exceptionFormat.replace("%EXCEPTION%", "{6}");
		exceptionFormat = exceptionFormat.replace("%STACKTRACE%", "{7}");

		this.exceptionPattern = exceptionFormat;

		exceptionMessageFormat = new MessageFormat(logPattern);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.util.logging.Formatter#format(java.util.logging.LogRecord)
	 */
	@Override
	public String format(LogRecord record)
	{
		Date time = new Date(record.getMillis());
		String formattedTime = dateFormat.format(time);

		String logMessage = "";

		if (record.getThrown() == null)
		{
			Object[] log =
			{ record.getLoggerName(), record.getLevel(), formattedTime, record.getMessage(), record.getSourceClassName(),
					record.getSourceMethodName(), record.getParameters() == null ? "" : record.getParameters()[0], record.getParameters() == null ? "" : record.getParameters()[1] };

			logMessage = logMessageFormat.format(log);
		}
		else
		{
			String stack = getStackLayout(record.getThrown(), "");

			Object[] log =
			{ record.getLoggerName(), record.getLevel(), formattedTime, record.getMessage(), record.getSourceClassName(),
					record.getSourceMethodName(), record.getThrown().getMessage(), stack };

			logMessage = exceptionMessageFormat.format(log);
		}
		return logMessage;
	}

	/**
	 * Gets the stack layout.
	 * 
	 * @param t
	 *            the t
	 * @param indenter
	 *            the indenter
	 * 
	 * @return the stack layout
	 */
	private String getStackLayout(Throwable t, String indenter)
	{
		indenter = indenter + "  ";

		StackTraceElement[] ste = t.getStackTrace();
		String stack = indenter + ste[0].toString();
		for (int i = 1; i < ste.length; i++)
		{
			stack = stack + "\n" + indenter + ste[i];
		}

		String innerStack = "";
		if (t.getCause() != null)
		{
			innerStack = indenter + "Caused by: " + t.getCause().getMessage() + "\n";
			innerStack = innerStack + getStackLayout(t.getCause(), indenter);
		}
		stack = stack + "\n" + innerStack;

		return stack;
	}

	/**
	 * Gets the exception pattern.
	 * 
	 * @return the exception pattern
	 */
	public String getExceptionPattern()
	{
		return exceptionPattern;
	}

	/**
	 * Gets the log pattern.
	 * 
	 * @return the log pattern
	 */
	public String getLogPattern()
	{
		return logPattern;
	}

	/**
	 * Gets the time format.
	 * 
	 * @return the time format
	 */
	public String getTimeFormat()
	{
		return timeFormat;
	}

}
