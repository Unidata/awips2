/*
 * Created on Jul 9, 2003
 *
 * 
 */
package ohd.hseb.util;

import java.io.*;

/**
 * @author Chip Gobs
 *
 * 
 */
public interface Logger
{
     public void log(String message);
     public void close();
     public PrintWriter getPrintWriter();
}
