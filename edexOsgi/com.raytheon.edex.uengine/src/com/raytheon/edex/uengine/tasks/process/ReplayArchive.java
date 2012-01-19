/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract DG133W-05-CQ-1067 with the US Government.
 * 
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 * 
 * Contractor Name:        Raytheon Company
 * Contractor Address:     6825 Pine Street, Suite 340
 *                         Mail Stop B8
 *                         Omaha, NE 68106
 *                         402.291.0100
 * 
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/
package com.raytheon.edex.uengine.tasks.process;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.GregorianCalendar;
import java.util.Iterator;
import java.util.List;
import java.util.TreeSet;

import org.apache.commons.lang.StringUtils;

import com.raytheon.edex.uengine.tasks.ScriptTask;
import com.raytheon.uf.common.message.response.ResponseMessageGeneric;
import com.raytheon.uf.edex.core.props.EnvProperties;
import com.raytheon.uf.edex.core.props.PropertiesFactory;

/**
 * This script task will start replaying the files located in the sbnDir.
 * While keeping the delays between each file.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 12/12/2007   561         dfitch      Initial Creation	
 * 18Sep2008    SP#12	    ebabin      update to not block caller.
 * 
 * </pre>
 * 
 * @author dfitch
 * @version 1
 */
public class ReplayArchive extends ScriptTask
{
    private static File sbnDir = null;

    static {
    	 EnvProperties env = PropertiesFactory.getInstance().getEnvProperties();
         String defaultDataDir = env.getEnvValue("DEFAULTDATADIR") + File.separatorChar + "sbn";
         sbnDir = new File(defaultDataDir);
    }
    
    private List<String> lstArchiveDirectories;

    private TreeSet<FileWithTime> sortedSet;

    public ReplayArchive()
    {
        super();
        lstArchiveDirectories = new ArrayList<String>();
        sortedSet = new TreeSet<FileWithTime>();
    }


    /**
     * @param lstFilesAndTimes
     */
    private void dumpFileTimes(List<FileWithTime> lstFilesAndTimes)
    {
        loginfo("*********************");
        for (int i = 0; i < lstFilesAndTimes.size(); ++i)
        {
            FileWithTime fwt = lstFilesAndTimes.get(i);
            loginfo("Mod Time: " + timeStr(fwt.getModifiedTime()) + "\t"
                    + "File: " + fwt.getFile());
        }
        loginfo("*********************");
    }

    
    /**
     * @param fwt --
     *            the file with time stamp info from the archive. The file is
     *            copied to a temporary file, the time stamp is set, and then
     */
    private boolean play(FileWithTime fwt)
    {
        File file = fwt.getFile();
        File tempFile = copyFileToTmp(file);
        tempFile.setLastModified(fwt.getModifiedTime());

        // important to look at the full path to determine where to put it
        // in the target sbn directory tree....
        String parts[] = StringUtils.split(file.toString(), File.separatorChar);
        String dataType = parts[parts.length - 2];
        loginfo("dataType=" + dataType);
        String newPath = sbnDir + "/" + dataType + "/" + file.getName();
        File newFile = new File(newPath);

        return tempFile.renameTo(newFile);  
    }

    /**
     * @param file
     */
    private File copyFileToTmp(File file)
    {
        File temp = null;
        try
        {
            temp = File.createTempFile("tmp", ".tmp");
            OutputStream out = new FileOutputStream(temp);
            InputStream is = new FileInputStream(file);
            byte[] bytes = new byte[8 * 1024];

            // Read in the bytes
            int numRead = 0;
            while (0 <= (numRead = is.read(bytes)))
            {
                out.write(bytes, 0, numRead);
            }
            // Close the input stream and return bytes
            is.close();
            out.close();
        }
        catch (IOException e)
        {
            e.printStackTrace();
            logwarn("Problem copying file " + file + " to " + temp, e);
        }
        return temp;
    }

    /**
     * @param modifiedTime
     * @return
     */
    static String timeStr(long modifiedTime)
    {
        String result = null;
        GregorianCalendar local = new GregorianCalendar();
        local.setTimeInMillis(modifiedTime);
        result = "" + local.getTime();
        return result;
    }

    // Process only files under dir
    private void visitAllFiles(File dir)
    {
        if (dir.isDirectory())
        {
            String[] children = dir.list();
            for (int i = 0; i < children.length; i++)
            {
                visitAllFiles(new File(dir, children[i]));
            }
        }
        else
        {
            processFile(dir);
        }
    }

    /**
     * @param dir
     */
    private void processFile(File file)
    {
        loginfo(file.toString());
        long modifiedTime = file.lastModified();
        sortedSet.add(new FileWithTime(file, modifiedTime));
    }

    /**
     * @param dir
     */
    private void loginfo(String msg)
    {
        logger.info(msg);
    }

    /**
     * @param string
     */
    private void logwarn(String msg)
    {
        logger.warn(msg);
    }

    /**
     * @param string
     * @param e
     */
    private void logwarn(String msg, IOException e)
    {
        logger.warn(msg, e);
    }

    /**
     * @param string
     */
    public void addArchiveDirectory(String directoryName)
    {
        lstArchiveDirectories.add(directoryName);

    }

    /**
     * @param fn
     */
    public void setSbnDir(String fn)
    {
        sbnDir = new File(fn);
    }

    /**
     * Replay the archive returning an XML string of some information about what
     * happened...
     * 
     * TODO: MGP this still needs to be tested some more...
     * TODO: MGP write a junit test for this!
     */
    @Override
    public Object execute()
    {
        // Form a string about what will be done.
    	ResponseMessageGeneric msg = new ResponseMessageGeneric();
    	
        String result = "Received ReplayArchive request, starting to replay the archive: "
            +sbnDir + " please check log for results.";

        msg.setContents(result);
        Replayer player = new Replayer();
        Thread myThread = new Thread(player);
        myThread.start();

        return msg;
    }

    class Replayer extends Thread {

    	public Replayer() {
    		super("Runner");
    	}
    	@Override
    	public void run() {
           
            for (Iterator<String> it = lstArchiveDirectories.iterator(); it.hasNext();)
            {
                String dirName = it.next();
                visitAllFiles(new File(dirName));
            }
            // Now sortedSet is filled with FileWithTime objects and we simply need
            // to
            // dequeue them in time order....
            List<FileWithTime> lstFilesAndTimes = new ArrayList<FileWithTime>();
            lstFilesAndTimes.addAll(sortedSet); 

            // Handle the special cases....
            int size = lstFilesAndTimes.size();
            FileWithTime fwt = lstFilesAndTimes.get(0);
            if (1 == size)
            {
            	play(fwt);
            }
            else
            {
                dumpFileTimes(lstFilesAndTimes);
                FileWithTime fwtNext = lstFilesAndTimes.get(1);
    	            long sleepTime = fwtNext.getModifiedTime() - fwt.getModifiedTime();
    	            try {
						Thread.sleep(sleepTime);
					} catch (InterruptedException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					}
                
                for (int i = 1; i < lstFilesAndTimes.size() - 1; ++i)
                {
                    fwt = lstFilesAndTimes.get(i);
                    loginfo("Mod Time: " + timeStr(fwt.getModifiedTime()) + "\t"
                            + "File: " + fwt.getFile());
                    // Calculate the sleep time....
                    play(lstFilesAndTimes.get(i));
                    fwtNext = lstFilesAndTimes.get(i + 1);
                    sleepTime = fwtNext.getModifiedTime() - fwt.getModifiedTime();
                    try {
						Thread.sleep(sleepTime);
					} catch (InterruptedException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					}
                }
                // Calculate the last sleep time....
                play(lstFilesAndTimes
                        .get(lstFilesAndTimes.size() - 1));
            }
    		
    	}
    }
    
}


class FileWithTime implements Comparable<FileWithTime>
{
    private File file;

    private long modifiedTime;

    /**
     * @param file
     * @param modifiedTime
     */
    public FileWithTime(File file, long modifiedTime)
    {
        super();
        this.file = file;
        this.modifiedTime = modifiedTime;
    }

    public String toString()
    {
        return ReplayArchive.timeStr(modifiedTime) + "\t" + file;
    }

    public int compareTo(FileWithTime o)
    {
        int result = 0;
        FileWithTime other = (FileWithTime) o;
        if (other.modifiedTime < this.modifiedTime)
        {
            result = 1;
        }
        else if (other.modifiedTime > this.modifiedTime)
        {
            result = -1;
        }
        else
        {
            result = 0;
        }

        return result;
    }

    /**
     * @return the file
     */
    public File getFile()
    {
        return file;
    }

    /**
     * @param file
     *            the file to set
     */
    public void setFile(File file)
    {
        this.file = file;
    }

    /**
     * @return the modifiedTime
     */
    public long getModifiedTime()
    {
        return modifiedTime;
    }

    /**
     * @param modifiedTime
     *            the modifiedTime to set
     */
    public void setModifiedTime(long modifiedTime)
    {
        this.modifiedTime = modifiedTime;
    }

}