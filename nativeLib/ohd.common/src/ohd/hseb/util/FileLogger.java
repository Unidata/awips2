/*
 * Created on Jul 9, 2003
 *
 * 
 */
package ohd.hseb.util;

import java.io.FileOutputStream;
import java.io.OutputStream;
import java.io.PrintWriter;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.TimeZone;

/**
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- -------------------------------------
 * May 13, 2011  8079       jnjanga     added custom printStackTrace method
 * 
 * </pre>
 * 
 * @author gobsc
 */
public class FileLogger implements Logger {

    // private vars
    private String _fileName = null;

    private OutputStream _outputStream = null;

    private PrintWriter _writer = null;

    private boolean _usingRealFile = false;

    private boolean _fileIsOpen = false;

    private static final String dateFormatString = "yyyy/MM/dd HH:mm:ss.SSS";

    private static final SimpleDateFormat _dateFormatter = new SimpleDateFormat(
            dateFormatString);

    static {
        _dateFormatter.setTimeZone(TimeZone.getTimeZone("UTC"));
    }

    // options
    private boolean _appendDateTime = false;

    private boolean _keepFileOpen = true;

    // ---------------------------------------------------------------------

    public FileLogger(boolean appendDateTime) {

        _appendDateTime = appendDateTime;

    }

    /* 
	 */
    public FileLogger(String fileName) {
        // default is to keep the file open until closed
        // and to append the date and time to each line
        // of text

        this(fileName, true, true);
    }

    // ---------------------------------------------------------------------

    public FileLogger(String fileName, boolean keepFileOpen,
            boolean appendDateTime) {
        _keepFileOpen = keepFileOpen;
        _appendDateTime = appendDateTime;

        _fileName = fileName;

    }

    // ---------------------------------------------------------------------

    public void log(String message) {

        // open if needed
        if (!isOpen()) {
            openFile(_fileName);
            _writer = new PrintWriter(_outputStream);
        }

        // write out the message, pre-pending date time stamp if required
        if (_appendDateTime) {
            _writer.println(getDateTimeStamp() + ": " + message);
        } else {
            _writer.println(message);
        }

        _writer.flush();

        // close if supposed to do so
        if (!_keepFileOpen) {
            close();
        }
    }

    /**
     * logs a stack trace
     * 
     * @param aThrowable
     *            a thrown exception
     */
    public void printStackTrace(Throwable aThrowable) {
        // add the class name and any message passed to constructor
        final String SPACE = " ";
        final StringBuilder result = new StringBuilder(SPACE);
        result.append(aThrowable.toString());
        final String NEW_LINE = System.getProperty("line.separator");
        result.append(NEW_LINE);

        // add each element of the stack trace
        for (StackTraceElement element : aThrowable.getStackTrace()) {
            result.append(NEW_LINE);
            for (int i = 0; i < 25; i++)
                result.append(SPACE);
            result.append(element);
        }
        log(result.toString());
    }

    // ---------------------------------------------------------------------
    public PrintWriter getPrintWriter() {
        return _writer;
    }

    // ---------------------------------------------------------------------

    private String getDateTimeStamp() {
        Date date = new Date();

        return _dateFormatter.format(date);
    }

    // ---------------------------------------------------------------------

    private void openFile(String fileName) {
        try {
            if (fileName != null) {
                _outputStream = new FileOutputStream(fileName, true);
                _usingRealFile = true;
                _fileIsOpen = true;
            } else // fileName == null
            {
                _outputStream = System.out;

            }

            _writer = new PrintWriter(_outputStream);

        } catch (java.io.IOException e) {
            e.printStackTrace();
        }
    }

    // ---------------------------------------------------------------------

    public void close() {
        // try
        {
            if (isOpen() && _usingRealFile) {
                _fileIsOpen = false;
                _writer.close();
            }
        }
        // catch(java.io.IOException e)
        {
            // e.printStackTrace();
        }
    }

    // ---------------------------------------------------------------------

    private boolean isOpen() {
        return _fileIsOpen;
    }

    // ---------------------------------------------------------------------

} // FileLogger