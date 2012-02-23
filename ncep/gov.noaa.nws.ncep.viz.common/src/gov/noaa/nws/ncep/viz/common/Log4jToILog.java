package gov.noaa.nws.ncep.viz.common;

import org.apache.log4j.AppenderSkeleton;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.apache.log4j.spi.LoggingEvent;
import org.eclipse.core.runtime.ILog;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;

public class Log4jToILog {

	/**
	 * This method adds an appender to the log4j root logger, which writes 
	 * log4j logging statements to Eclipse logging framework. All the messages
	 * will be redirected and finally appear in the alertviz bar.
	 * 
	 * @param log
	 * @param PLUGIN_ID
	 */
	public static void setup(final ILog log, final String PLUGIN_ID) {

		Logger.getRootLogger().addAppender(new AppenderSkeleton() {

			@Override
			protected void append(LoggingEvent arg0) {
				Level level = arg0.getLevel();

				Throwable t = arg0.getThrowableInformation() == null ? null : arg0
						.getThrowableInformation().getThrowable();
				if (t == null) {
					log.log(new Status(toIStatusLevel(level), PLUGIN_ID, arg0.getRenderedMessage()));
				} else {
					log.log(new Status(toIStatusLevel(level), PLUGIN_ID, arg0.getRenderedMessage(), t));
				}
			}

			@Override
			public void close() {
				// do nothing
			}

			@Override
			public boolean requiresLayout() {
				return false;
			}
		});
	}

	/**
	 * Transform log4j logging level to iStatus logging level.
	 * 
	 * @param level
	 * @return
	 */
	private static int toIStatusLevel(Level level) {
		int istatus;
		switch (level.toInt()) {
		case Level.TRACE_INT:
			istatus = IStatus.OK;
			break;
		case Level.DEBUG_INT:
			istatus = IStatus.OK;
			break;
		case Level.INFO_INT:
			istatus = IStatus.INFO;
			break;
		case Level.WARN_INT:
			istatus = IStatus.WARNING;
			break;
		case Level.ERROR_INT:
			istatus = IStatus.ERROR;
			break;
		case Level.FATAL_INT:
			istatus = IStatus.ERROR;
			break;
		default:
			istatus = IStatus.OK;
		}
		return istatus;
	}
}
