/////////////////////////////////////////////////////////////////////////////
// Copyright (c) 1998, California Institute of Technology.
// ALL RIGHTS RESERVED.   U.S. Government Sponsorship acknowledged.
//
// Please read the full copyright notice in the file COPYRIGHT
// in this directory.
//
// Author: Jake Hamby, NASA/Jet Propulsion Laboratory
//         Jake.Hamby@jpl.nasa.gov
/////////////////////////////////////////////////////////////////////////////

package dods.dap;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.DataInputStream;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.zip.InflaterInputStream;

import com.raytheon.dods.HttpConnectStrategy;

import dods.dap.parser.ParseException;

/**
 * This class provides support for common DODS client-side operations such as
 * dereferencing a DODS URL, communicating network activity status to the user
 * and reading local DODS objects.
 * <p>
 * Unlike its C++ counterpart, this class does not store instances of the DAS,
 * DDS, etc. objects. Rather, the methods <code>getDAS</code>, etc. return
 * instances of those objects.
 * 
 * @version $Revision: 1.9 $
 * @author jehamby
 */
public class DConnect {
    static final String DODS_SOCKET_TIMEOUT_MILLISECONDS = "dods.socket.timeout.milliseconds";

    static final String DODS_CONNECTION_TIMEOUT_MILLISECONDS = "dods.connection.timeout.milliseconds";

    private static final int MAX_NUMBER_OF_URL_ATTEMPTS_TO_MAKE = 4;

    private static final int CONNECTION_TIMEOUT = Integer.getInteger(
            DODS_CONNECTION_TIMEOUT_MILLISECONDS, 0);

    private static final int SOCKET_TIMEOUT = Integer.getInteger(
            DODS_SOCKET_TIMEOUT_MILLISECONDS, 0);

    private static final String HTTP_CONNECT_STRATEGY = System
            .getProperty("dods.http.connect.strategy");

    private final boolean dumpStream = 
        System.getProperties().containsKey("dods.dap.dumpStream");

    /** InputStream to use for connection to a file instead of a remote host. */
    private InputStream fileStream;

    /**
     * The current DODS URL, as a String (will be converted to URL inside of
     * getDAS(), getDDS(), and getData()), without Constraint Expression.
     */
    private String urlString;

    /** The projection portion of the current DODS CE (including leading "?"). */
    private String projString;

    /** The selection portion of the current DODS CE (including leading "&"). */
    private String selString;

    /** The DODS server version. */
    private ServerVersion ver;

    private final HttpConnectStrategy httpStrategy;

    /**
     * Creates an instance bound to url which accepts compressed documents.
     * 
     * @param urlString
     *            connect to this URL.
     * @exception FileNotFoundException
     *                thrown if <code>urlString</code> is not a valid URL, or a
     *                filename which exists on the system.
     * @see DConnect#DConnect(String, boolean)
     */
    public DConnect(String urlString) throws FileNotFoundException {
        this(urlString, true);
    }

    /**
     * Creates an instance bound to url which accepts compressed documents.
     * 
     * @param urlString
     *            connect to this URL.
     * @exception FileNotFoundException
     *                thrown if <code>urlString</code> is not a valid URL, or a
     *                filename which exists on the system.
     * @see DConnect#DConnect(String, boolean)
     */
    public DConnect(String urlString, boolean acceptDeflate)
            throws FileNotFoundException {
        this(urlString, null, null, acceptDeflate);
    }

    /**
     * Creates an instance bound to url which accepts compressed documents.
     * 
     * @param urlString
     *            connect to this URL.
     * @param proxyHost
     *            the proxy host
     * @param proxyPort
     *            the proxy port
     * @exception FileNotFoundException
     *                thrown if <code>urlString</code> is not a valid URL, or a
     *                filename which exists on the system.
     * @see DConnect#DConnect(String, boolean)
     */
    public DConnect(String urlString, String proxyHost, String proxyPort)
            throws FileNotFoundException {
        this(urlString, proxyHost, proxyPort, true);
    }

    /**
     * Creates an instance bound to url. If <code>acceptDeflate</code> is true
     * then HTTP Request headers will indicate to servers that this client can
     * accept compressed documents.
     * 
     * @param urlString
     *            Connect to this URL. If urlString is not a valid URL, it is
     *            assumed to be a filename, which is opened.
     * @param proxyHost
     *            the proxy host
     * @param proxyPort
     *            the proxy port
     * @param acceptDeflate
     *            true if this client can accept responses encoded with deflate.
     * @exception FileNotFoundException
     *                thrown if <code>urlString</code> is not a valid URL, or a
     *                filename which exists on the system.
     */
    public DConnect(String urlString, String proxyHost, String proxyPort,
            boolean acceptDeflate) throws FileNotFoundException {
        this(urlString, proxyHost, proxyPort, acceptDeflate,
                newInstanceOfAssignableType(HttpConnectStrategy.class,
                        HTTP_CONNECT_STRATEGY));
    }

    /**
     * Package-private version of the constructor which allows the httpStrategy
     * to be injected.
     * 
     * @param urlString
     *            Connect to this URL. If urlString is not a valid URL, it is
     *            assumed to be a filename, which is opened.
     * @param proxyHost
     *            the proxy host
     * @param proxyPort
     *            the proxy port
     * @param acceptDeflate
     *            true if this client can accept responses encoded with deflate.
     * @param httpStrategy
     *            the http strategy to use
     * @exception FileNotFoundException
     *                thrown if <code>urlString</code> is not a valid URL, or a
     *                filename which exists on the system.
     */
    DConnect(String urlString, String proxyHost, String proxyPort,
            boolean acceptDeflate, HttpConnectStrategy httpStrategy)
            throws FileNotFoundException {

        this.httpStrategy = httpStrategy;
        // Prevent connections from having unlimited time (unless using default
        // values)
        httpStrategy.setConnectionTimeout(CONNECTION_TIMEOUT);
        httpStrategy.setSocketTimeout(SOCKET_TIMEOUT);

        if (proxyHost != null && proxyPort != null) {
            httpStrategy.setProxy(proxyHost, Integer.parseInt(proxyPort));
        }

        int ceIndex = urlString.indexOf('?');
        if (ceIndex != -1) {
            this.urlString = urlString.substring(0, ceIndex);
            String expr = urlString.substring(ceIndex);
            int selIndex = expr.indexOf('&');
            if (selIndex != -1) {
                this.projString = expr.substring(0, selIndex);
                this.selString = expr.substring(selIndex);
            } else {
                this.projString = expr;
                this.selString = "";
            }
        } else {
            this.urlString = urlString;
            this.projString = this.selString = "";
        }
        // Test if the URL is really a filename, and if so, open the file
        try {
            new URL(urlString);
        } catch (MalformedURLException e) {
            fileStream = new FileInputStream(urlString);
        }
    }

    /**
     * Creates an instance bound to an already open <code>InputStream</code>.
     * 
     * @param is
     *            the <code>InputStream</code> to open.
     */
    public DConnect(InputStream is) {
        this.fileStream = is;
        this.httpStrategy = null;
    }

    /**
     * Returns whether a file name or <code>InputStream</code> is being used
     * instead of a URL.
     * 
     * @return true if a file name or <code>InputStream</code> is being used.
     */
    public final boolean isLocal() {
        return (fileStream != null);
    }

    /**
     * Returns the constraint expression supplied with the URL given to the
     * constructor. If no CE was given this returns an empty <code>String</code>
     * .
     * <p>
     * Note that the CE supplied to one of this object's constructors is
     * "sticky"; it will be used with every data request made with this object.
     * The CE passed to <code>getData</code>, however, is not sticky; it is used
     * only for that specific request. This method returns the sticky CE.
     * 
     * @return the constraint expression associated with this connection.
     */
    public final String CE() {
        return projString + selString;
    }

    /**
     * Returns the URL supplied to the constructor. If the URL contained a
     * constraint expression that is not returned.
     * 
     * @return the URL of this connection.
     */
    public final String URL() {
        return urlString;
    }

    /**
     * Returns the `Data object' from the dataset referenced by this object's
     * URL given the constraint expression CE. Note that the Data object is
     * really just a DDS object with data bound to the variables. The DDS will
     * probably contain fewer variables (and those might have different types)
     * than in the DDS returned by getDDS() because that method returns the
     * entire DDS (but without any data) while this method returns only those
     * variables listed in the projection part of the constraint expression.
     * <p>
     * Note that if CE is an empty String then the entire dataset will be
     * returned, unless a "sticky" CE has been specified in the constructor.
     * 
     * @param CE
     *            The constraint expression to be applied to this request by the
     *            server. This is combined with any CE given in the constructor.
     * @param statusUI
     *            the <code>StatusUI</code> object to use for GUI updates and
     *            user cancellation notification (may be null).
     * @return The <code>DataDDS</code> object that results from applying the
     *         given CE, combined with this object's sticky CE, on the
     *         referenced dataset.
     * @exception MalformedURLException
     *                if the URL given to the constructor has an error
     * @exception IOException
     *                if any error connecting to the remote server
     * @exception ParseException
     *                if the DDS parser returned an error
     * @exception DDSException
     *                on an error constructing the DDS
     * @exception DODSException
     *                if any error returned by the remote server
     */
    public DataDDS getData(String CE, StatusUI statusUI, BaseTypeFactory btf)
            throws MalformedURLException, IOException, ParseException,
            DDSException, DODSException {

        if (fileStream != null)
            return getDataFromFileStream(fileStream, statusUI, btf);

        String localProjString, localSelString;
        int selIndex = CE.indexOf('&');
        if (selIndex != -1) {
            localProjString = CE.substring(0, selIndex);
            localSelString = CE.substring(selIndex);
        } else {
            localProjString = CE;
            localSelString = "";
        }
        URL url = new URL(urlString + ".dods" + projString + localProjString
                + selString + localSelString);

        String errorMsg = "DConnect getData failed " + url;
        int errorCode = DODSException.UNKNOWN_ERROR;
        int retry = 1;
        long backoff = 100L;
        while (true) {
            try {
                return getDataFromUrl(url, statusUI, btf);
            } catch (DODSException e) {
                System.out.println("DConnect getData failed; retry (" + retry
                        + "," + backoff + ") " + url);
                errorMsg = e.getErrorMessage();
                errorCode = e.getErrorCode();

                try {
                    Thread.sleep(backoff);
                } catch (InterruptedException ie) {
                }
            }

            if (retry == 5)
                throw new DODSException(errorCode, errorMsg);
            retry++;
            backoff *= 2;
        }
    }

    private DataDDS getDataFromFileStream(InputStream fileStream,
            StatusUI statusUI, BaseTypeFactory btf) throws IOException,
            ParseException, DDSException, DODSException {

        InputStream is = parseMime(fileStream);
        DataDDS dds = new DataDDS(ver, btf);

        try {
            dds.parse(new HeaderInputStream(is)); // read the DDS header
            // NOTE: the HeaderInputStream will have skipped over "Data:" line
            dds.readData(is, statusUI); // read the data!

        } finally {
            is.close(); // stream is always closed even if parse() throws
                        // exception
        }
        return dds;
    }

    
    // DEBUG JC
    private void copy(InputStream in, OutputStream out) {
        try {
            byte[] buffer = new byte[256];
            while (true) {
                int bytesRead = in.read(buffer);
                if (bytesRead == -1)
                    break;
                out.write(buffer, 0, bytesRead);
            }
        } catch (IOException e) {
        }
    }

    // DEBUG JC
    private void dump(InputStream is) throws IOException {
        DataInputStream d = new DataInputStream(is);

        try {
            // System.out.println("dump lines avail=" + is.available());
            while (true) {
                @SuppressWarnings("deprecation")
                String line = d.readLine();
                System.out.println(line);
                if (null == line)
                    return;
                if (line.equals("Data:"))
                    break;
            }
            // System.out.println("dump bytes avail=" + is.available());
            dumpBytes(is, 20);

        } catch (java.io.EOFException e) {
        }
    }

    private void dumpBytes(InputStream is, int n) {
        try {
            DataInputStream d = new DataInputStream(is);
            int count = 0;
            while ((count < n) && (d.available() > 0)) {
                // System.out.println(count + " " + d.readByte());
                count++;
            }
        } catch (java.io.IOException e) {
        }
    }

    /**
     * Returns the `Data object' from the dataset referenced by this object's
     * URL given the constraint expression CE. Note that the Data object is
     * really just a DDS object with data bound to the variables. The DDS will
     * probably contain fewer variables (and those might have different types)
     * than in the DDS returned by getDDS() because that method returns the
     * entire DDS (but without any data) while this method returns only those
     * variables listed in the projection part of the constraint expression.
     * <p>
     * Note that if CE is an empty String then the entire dataset will be
     * returned, unless a "sticky" CE has been specified in the constructor.
     * 
     * @param CE
     *            The constraint expression to be applied to this request by the
     *            server. This is combined with any CE given in the constructor.
     * @param statusUI
     *            the <code>StatusUI</code> object to use for GUI updates and
     *            user cancellation notification (may be null).
     * @return The <code>DataDDS</code> object that results from applying the
     *         given CE, combined with this object's sticky CE, on the
     *         referenced dataset.
     * @exception MalformedURLException
     *                if the URL given to the constructor has an error
     * @exception IOException
     *                if any error connecting to the remote server
     * @exception ParseException
     *                if the DDS parser returned an error
     * @exception DDSException
     *                on an error constructing the DDS
     * @exception DODSException
     *                if any error returned by the remote server
     */
    public DataDDS getData(String CE, StatusUI statusUI)
            throws MalformedURLException, IOException, ParseException,
            DDSException, DODSException {

        return getData(CE, statusUI, new DefaultFactory());
    }

    /**
     * Return the data object with no local constraint expression. Same as
     * <code>getData("", statusUI)</code>.
     * 
     * @param statusUI
     *            the <code>StatusUI</code> object to use for GUI updates and
     *            user cancellation notification (may be null).
     * @return The <code>DataDDS</code> object that results from applying this
     *         object's sticky CE, if any, on the referenced dataset.
     * @exception MalformedURLException
     *                if the URL given to the constructor has an error
     * @exception IOException
     *                if any error connecting to the remote server
     * @exception ParseException
     *                if the DDS parser returned an error
     * @exception DDSException
     *                on an error constructing the DDS
     * @exception DODSException
     *                if any error returned by the remote server
     * @see DConnect#getData(String, StatusUI)
     */
    public final DataDDS getData(StatusUI statusUI)
            throws MalformedURLException, IOException, ParseException,
            DDSException, DODSException {
        return getData("", statusUI, new DefaultFactory());
    }

    /**
     * Returns the <code>ServerVersion</code> of the last connection.
     * 
     * @return the <code>ServerVersion</code> of the last connection.
     */
    public final ServerVersion getServerVersion() {
        return ver;
    }

    /**
     * A primitive parser for the MIME headers used by DODS. This is used when
     * reading from local sources of DODS Data objects. It is called by
     * <code>readData</code> to simulate the important actions of the
     * <code>URLConnection</code> MIME header parsing performed in
     * <code>openConnection</code> for HTTP URL's.
     * <p>
     * <b><i>NOTE:</b></i> Because BufferedReader seeks ahead, and therefore
     * removescharacters from the InputStream which are needed later, and
     * because there is no way to construct an InputStream from a
     * BufferedReader, we have to use DataInputStream to read the header lines,
     * which triggers an unavoidable deprecated warning from the Java compiler.
     * 
     * @param is
     *            the InputStream to read.
     * @return the InputStream to read data from (after attaching any necessary
     *         decompression filters).
     * @exception IOException
     *                if any IO error.
     * @exception DODSException
     *                if the server returned an Error.
     */
    @SuppressWarnings("deprecation")
    private InputStream parseMime(InputStream is) throws IOException,
            DODSException {

        // NOTE: because BufferedReader seeks ahead, and therefore removes
        // characters from the InputStream which are needed later, and
        // because there is no way to construct an InputStream from a
        // BufferedReader, we have to use DataInputStream to read the header
        // lines, which triggers an unavoidable deprecated warning from the
        // Java compiler.

        DataInputStream d = new DataInputStream(is);

        String description = null;
        String encoding = null;

        // while there are more header (non-blank) lines
        String line;
        while (!(line = d.readLine()).equals("")) {
            int spaceIndex = line.indexOf(' ');
            // all header lines should have a space in them, but if not, skip
            // ahead
            if (spaceIndex == -1)
                continue;
            String header = line.substring(0, spaceIndex);
            String value = line.substring(spaceIndex + 1);

            if (header.equals("Server:")) {
                ver = new ServerVersion(value);
            } else if (header.equals("Content-Description:")) {
                description = value;
            } else if (header.equals("Content-Encoding:")) {
                encoding = value;
            }
        }
        handleContentDesc(is, description);
        return handleContentEncoding(is, encoding);
    }

    /**
     * This code handles the Content-Description: header for
     * <code>openConnection</code> and <code>parseMime</code>. Throws a
     * <code>DODSException</code> if the type is <code>dods_error</code>.
     * 
     * @param is
     *            the InputStream to read.
     * @param type
     *            the Content-Description header, or null.
     * @exception IOException
     *                if any error reading from the server.
     * @exception DODSException
     *                if the server returned an error.
     */
    public static void handleContentDesc(InputStream is, String type)
            throws IOException, DODSException {
        if (type != null && type.equals("dods_error")) {
            // create server exception object
            DODSException ds = new DODSException();
            // parse the Error object from stream and throw it
            ds.parse(is);
            throw ds;
        }
    }

    /**
     * This code handles the Content-type: header for
     * <code>openConnection</code> and <code>parseMime</code>
     * 
     * @param is
     *            the InputStream to read.
     * @param encoding
     *            the Content-type header, or null.
     * @return the new InputStream, after applying an
     *         <code>InflaterInputStream</code> filter if necessary.
     */
    public static InputStream handleContentEncoding(InputStream is,
            String encoding) {
        if (encoding != null && encoding.equals("deflate")) {
            return new InflaterInputStream(is);
        } else {
            return is;
        }
    }

    /**
     * Returns the DDS object from the dataset referenced by this object's URL.
     * The DDS object is referred to by appending `.dds' to the end of a DODS
     * URL.
     * 
     * @return the DDS associated with the referenced dataset.
     * @exception MalformedURLException
     *                if the URL given to the constructor has an error
     * @exception IOException
     *                if an error connecting to the remote server
     * @exception ParseException
     *                if the DDS parser returned an error
     * @exception DDSException
     *                on an error constructing the DDS
     * @exception DODSException
     *                if an error returned by the remote server
     */
    public DDS getDDS() throws MalformedURLException, IOException,
            ParseException, DDSException, DODSException {

        InputStream is;

        if (fileStream != null)
            is = parseMime(fileStream);
        else {
            URL url = new URL(urlString + ".dds" + projString + selString);
            is = getInputStream(url);
        }

        DDS dds = new DDS();
        try {
            dds.parse(is);
        } finally {
            // stream is always closed even if parse() throws exception
            if (is != null)
                is.close();
        }
        return dds;
    }

    /**
     * Returns the DAS object from the dataset referenced by this object's URL.
     * The DAS object is referred to by appending `.das' to the end of a DODS
     * URL.
     * 
     * @return the DAS associated with the referenced dataset.
     * @exception MalformedURLException
     *                if the URL given to the constructor has an error
     * @exception IOException
     *                if an error connecting to the remote server
     * @exception ParseException
     *                if the DAS parser returned an error
     * @exception DASException
     *                on an error constructing the DAS
     * @exception DODSException
     *                if an error returned by the remote server
     */
    public DAS getDAS() throws MalformedURLException, IOException,
            ParseException, DASException, DODSException {
        InputStream is;

        if (fileStream != null)
            is = parseMime(fileStream);
        else {
            URL url = new URL(urlString + ".das" + projString + selString);
            is = getInputStream(url);
        }

        DAS das = new DAS();
        
        try {
            das.parse(is);
        } finally {
            if (is != null)
                is.close(); // stream is always closed even if parse() throws
                        // exception

        }
        return das;
    }

    /**
     * Handles retrieving an {@link InputStream} for a {@link URL}. Retry logic
     * is neutral of http connection strategy.
     * 
     * @param url
     * @return
     * @throws DODSException
     * @throws IOException
     */
    private InputStream getInputStream(java.net.URL url) throws IOException,
            DODSException {

        int attempts = 0;
        InputStream is = null;

        // Attempt to connect up to 3 times...
        do {
            is = httpStrategy.getInputStream(url);

            if (is != null) {
                // Get the current server version
                ver = httpStrategy.getServerVersion();

                // Then return the input stream
                return is;
            }

            // Status is not OK, log it.
            System.out.println("DConnect::getInputStream() attempt ["
                    + attempts + "] to URL [" + url.toExternalForm() + "]");

            // Sleep a little longer each attempt..
            try {
                Thread.sleep((attempts++) * 1000);
            } catch (InterruptedException e) {
                // Oh well, we retry a little sooner than we wanted
            }
        } while (attempts < MAX_NUMBER_OF_URL_ATTEMPTS_TO_MAKE);

        throw new IOException("Failed to open connection to URL: "
                + url.toExternalForm());
    }

    public DataDDS getDataFromUrl(URL url, StatusUI statusUI, BaseTypeFactory btf)
    throws MalformedURLException, IOException, ParseException, DDSException, DODSException {
    
        InputStream is = getInputStream(url);
        DataDDS dds = new DataDDS(ver, btf);
    
        // DEBUG
        ByteArrayInputStream bis = null;
        if (dumpStream) {
            System.out.println("DConnect to " + url);
            ByteArrayOutputStream bos = new ByteArrayOutputStream();
            copy(is, bos);
            bis = new ByteArrayInputStream(bos.toByteArray());
            is = bis;
        }
    
        try {
    
            if (dumpStream) {
                bis.mark(1000);
                System.out.println("DConnect parse header: ");
                dump(bis);
                bis.reset();
            }
    
            dds.parse(new HeaderInputStream(is)); // read the DDS header
            // NOTE: the HeaderInputStream will have skipped over "Data:" line
    
            if (dumpStream) {
                bis.mark(20);
                System.out
                        .println("DConnect done with header, next bytes are: ");
                dumpBytes(bis, 20);
                bis.reset();
            }
    
            dds.readData(is, statusUI); // read the data!
    
        } catch (Exception e) {
            System.out.println("DConnect dds.parse: " + url + "\n " + e);
            e.printStackTrace();
            /*
             * DEBUG if (dumpStream) { System.out.println("DConnect dump "+url);
             * bis.reset(); dump(bis); bis.reset(); File saveFile = null; try {
             * saveFile = File.createTempFile("debug","tmp", new File("."));
             * System.out.println("try Save file = "+
             * saveFile.getAbsolutePath()); FileOutputStream saveFileOS = new
             * FileOutputStream(saveFile); copy(bis, saveFileOS);
             * saveFileOS.close(); System.out.println("wrote Save file = "+
             * saveFile.getAbsolutePath()); } catch (java.io.IOException ioe) {
             * System.out.println("failed Save file = "+
             * saveFile.getAbsolutePath()); ioe.printStackTrace(); } }
             */
    
            throw new DODSException("Connection cannot be read " + url);
    
        } finally {
            // Stream is always closed even if parse() throws an Exception
            is.close();
        }

        return dds;
    }

    private static <T> T newInstanceOfAssignableType(Class<T> assignableClass,
            String name) {
        try {
            @SuppressWarnings("unchecked")
            final Class<? extends T> forName = (Class<? extends T>) Class
                    .forName(name);
            return assignableClass.cast(forName.newInstance());
        } catch (Exception e) {
            throw new IllegalArgumentException(String.format(
                    "%s is not assignable to a field of type %s", name,
                    assignableClass.getName()), e);
        }
    }
}
