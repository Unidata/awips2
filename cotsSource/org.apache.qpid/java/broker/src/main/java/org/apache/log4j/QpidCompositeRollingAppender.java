/*
 *
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 *
 */
package org.apache.log4j;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.Writer;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.Executor;
import java.util.concurrent.Executors;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.zip.GZIPOutputStream;

import org.apache.log4j.helpers.CountingQuietWriter;
import org.apache.log4j.helpers.LogLog;
import org.apache.log4j.helpers.OptionConverter;
import org.apache.log4j.spi.LoggingEvent;

/**
 * <p>CompositeRollingAppender combines RollingFileAppender and DailyRollingFileAppender<br> It can function as either
 * or do both at the same time (making size based rolling files like RollingFileAppender until a data/time boundary is
 * crossed at which time it rolls all of those files as per the DailyRollingFileAppender) based on the setting for
 * <code>rollingStyle</code>.<br> <br> To use CompositeRollingAppender to roll log files as they reach a certain size
 * (like RollingFileAppender), set rollingStyle=1 (@see config.size)<br> To use CompositeRollingAppender to roll log
 * files at certain time intervals (daily for example), set rollingStyle=2 and a datePattern (@see config.time)<br> To
 * have CompositeRollingAppender roll log files at a certain size AND rename those according to time intervals, set
 * rollingStyle=3 (@see config.composite)<br>
 *
 * <p>A of few additional optional features have been added:<br> -- Attach date pattern for current log file (@see
 * staticLogFileName)<br> -- Backup number increments for newer files (@see countDirection)<br> -- Infinite number of
 * backups by file size (@see maxSizeRollBackups)<br> <br> <p>A few notes and warnings:  For large or infinite number of
 * backups countDirection > 0 is highly recommended, with staticLogFileName = false if time based rolling is also used
 * -- this will reduce the number of file renamings to few or none.  Changing staticLogFileName or countDirection
 * without clearing the directory could have nasty side effects.  If Date/Time based rolling is enabled,
 * CompositeRollingAppender will attempt to roll existing files in the directory without a date/time tag based on the
 * last modified date of the base log files last modification.<br> <br> <p>A maximum number of backups based on
 * date/time boundries would be nice but is not yet implemented.<br>
 *
 * @author Kevin Steppe
 * @author Heinz Richter
 * @author Eirik Lygre
 * @author Ceki G&uuml;lc&uuml;
 * @author Martin Ritchie
 */
public class QpidCompositeRollingAppender extends FileAppender
{
    // The code assumes that the following 'time' constants are in a increasing
    // sequence.
    static final int TOP_OF_TROUBLE = -1;
    static final int TOP_OF_MINUTE = 0;
    static final int TOP_OF_HOUR = 1;
    static final int HALF_DAY = 2;
    static final int TOP_OF_DAY = 3;
    static final int TOP_OF_WEEK = 4;
    static final int TOP_OF_MONTH = 5;

    /** Style of rolling to use */
    static final int BY_SIZE = 1;
    static final int BY_DATE = 2;
    static final int BY_COMPOSITE = 3;

    // Not currently used
    static final String S_BY_SIZE = "Size";
    static final String S_BY_DATE = "Date";
    static final String S_BY_COMPOSITE = "Composite";

    /** The date pattern. By default, the pattern is set to "'.'yyyy-MM-dd" meaning daily rollover. */
    private String datePattern = "'.'yyyy-MM-dd";

    /**
     * The actual formatted filename that is currently being written to or will be the file transferred to on roll over
     * (based on staticLogFileName).
     */
    private String scheduledFilename = null;

    /** The timestamp when we shall next recompute the filename. */
    private long nextCheck = System.currentTimeMillis() - 1;

    /** Holds date of last roll over */
    Date now = new Date();

    SimpleDateFormat sdf;

    /** Helper class to determine next rollover time */
    RollingCalendar rc = new RollingCalendar();

    /** Current period for roll overs */
    int checkPeriod = TOP_OF_TROUBLE;

    /** The default maximum file size is 10MB. */
    protected long maxFileSize = 10 * 1024 * 1024;

    /** There is zero backup files by default. */
    protected int maxSizeRollBackups = 0;
    /** How many sized based backups have been made so far */
    protected int curSizeRollBackups = 0;

    /** not yet implemented */
    protected int maxTimeRollBackups = -1;
    protected int curTimeRollBackups = 0;

    /**
     * By default newer files have lower numbers. (countDirection < 0) ie. log.1 is most recent, log.5 is the 5th
     * backup, etc... countDirection > 0 does the opposite ie. log.1 is the first backup made, log.5 is the 5th backup
     * made, etc. For infinite backups use countDirection > 0 to reduce rollOver costs.
     */
    protected int countDirection = -1;

    /** Style of rolling to Use.  BY_SIZE (1), BY_DATE(2), BY COMPOSITE(3) */
    protected int rollingStyle = BY_COMPOSITE;
    protected boolean rollDate = true;
    protected boolean rollSize = true;

    /**
     * By default file.log is always the current file.  Optionally file.log.yyyy-mm-dd for current formated datePattern
     * can by the currently logging file (or file.log.curSizeRollBackup or even file.log.yyyy-mm-dd.curSizeRollBackup)
     * This will make time based roll overs with a large number of backups much faster -- it won't have to rename all
     * the backups!
     */
    protected boolean staticLogFileName = true;

    /** FileName provided in configuration.  Used for rolling properly */
    protected String baseFileName;

    /** Do we want to .gz our backup files. */
    protected boolean compress = false;

    /** Do we want to use a second thread when compressing our backup files. */
    protected boolean compressAsync = false;

    /** Do we want to start numbering files at zero. */
    protected boolean zeroBased = false;

    /** Path provided in configuration.  Used for moving backup files to */
    protected String backupFilesToPath = null;
    private final ConcurrentLinkedQueue<CompressJob> _compress = new ConcurrentLinkedQueue<CompressJob>();
    private AtomicBoolean _compressing = new AtomicBoolean(false);
    private static final String COMPRESS_EXTENSION = ".gz";

    /** The default constructor does nothing. */
    public QpidCompositeRollingAppender()
    { }

    /**
     * Instantiate a <code>CompositeRollingAppender</code> and open the file designated by <code>filename</code>. The
     * opened filename will become the ouput destination for this appender.
     */
    public QpidCompositeRollingAppender(Layout layout, String filename, String datePattern) throws IOException
    {
        this(layout, filename, datePattern, true);
    }

    /**
     * Instantiate a CompositeRollingAppender and open the file designated by <code>filename</code>. The opened filename
     * will become the ouput destination for this appender.
     *
     * <p>If the <code>append</code> parameter is true, the file will be appended to. Otherwise, the file desginated by
     * <code>filename</code> will be truncated before being opened.
     */
    public QpidCompositeRollingAppender(Layout layout, String filename, boolean append) throws IOException
    {
        super(layout, filename, append);
    }

    /**
     * Instantiate a CompositeRollingAppender and open the file designated by <code>filename</code>. The opened filename
     * will become the ouput destination for this appender.
     */
    public QpidCompositeRollingAppender(Layout layout, String filename, String datePattern, boolean append)
        throws IOException
    {
        super(layout, filename, append);
        this.datePattern = datePattern;
        activateOptions();
    }

    /**
     * Instantiate a CompositeRollingAppender and open the file designated by <code>filename</code>. The opened filename
     * will become the output destination for this appender.
     *
     * <p>The file will be appended to.  DatePattern is default.
     */
    public QpidCompositeRollingAppender(Layout layout, String filename) throws IOException
    {
        super(layout, filename);
    }

    /**
     * The <b>DatePattern</b> takes a string in the same format as expected by {@link java.text.SimpleDateFormat}. This
     * options determines the rollover schedule.
     */
    public void setDatePattern(String pattern)
    {
        datePattern = pattern;
    }

    /** Returns the value of the <b>DatePattern</b> option. */
    public String getDatePattern()
    {
        return datePattern;
    }

    /** Returns the value of the <b>maxSizeRollBackups</b> option. */
    public int getMaxSizeRollBackups()
    {
        return maxSizeRollBackups;
    }

    /**
     * Get the maximum size that the output file is allowed to reach before being rolled over to backup files.
     *
     * @since 1.1
     */
    public long getMaximumFileSize()
    {
        return maxFileSize;
    }

    /**
     * <p>Set the maximum number of backup files to keep around based on file size.
     *
     * <p>The <b>MaxSizeRollBackups</b> option determines how many backup files are kept before the oldest is erased.
     * This option takes an integer value. If set to zero, then there will be no backup files and the log file will be
     * truncated when it reaches <code>MaxFileSize</code>.  If a negative number is supplied then no deletions will be
     * made.  Note that this could result in very slow performance as a large number of files are rolled over unless
     * {@link #setCountDirection} up is used.
     *
     * <p>The maximum applys to -each- time based group of files and -not- the total. Using a daily roll the maximum
     * total files would be (#days run) * (maxSizeRollBackups)
     */
    public void setMaxSizeRollBackups(int maxBackups)
    {
        maxSizeRollBackups = maxBackups;
    }

    /**
     * Set the maximum size that the output file is allowed to reach before being rolled over to backup files.
     *
     * <p>This method is equivalent to {@link #setMaxFileSize} except that it is required for differentiating the setter
     * taking a <code>long</code> argument from the setter taking a <code>String</code> argument by the JavaBeans {@link
     * java.beans.Introspector Introspector}.
     *
     * @see #setMaxFileSize(String)
     */
    public void setMaxFileSize(long maxFileSize)
    {
        this.maxFileSize = maxFileSize;
    }

    /**
     * Set the maximum size that the output file is allowed to reach before being rolled over to backup files.
     *
     * <p>This method is equivalent to {@link #setMaxFileSize} except that it is required for differentiating the setter
     * taking a <code>long</code> argument from the setter taking a <code>String</code> argument by the JavaBeans {@link
     * java.beans.Introspector Introspector}.
     *
     * @see #setMaxFileSize(String)
     */
    public void setMaximumFileSize(long maxFileSize)
    {
        this.maxFileSize = maxFileSize;
    }

    /**
     * Set the maximum size that the output file is allowed to reach before being rolled over to backup files.
     *
     * <p>In configuration files, the <b>MaxFileSize</b> option takes an long integer in the range 0 - 2^63. You can
     * specify the value with the suffixes "KB", "MB" or "GB" so that the integer is interpreted being expressed
     * respectively in kilobytes, megabytes or gigabytes. For example, the value "10KB" will be interpreted as 10240.
     */
    public void setMaxFileSize(String value)
    {
        maxFileSize = OptionConverter.toFileSize(value, maxFileSize + 1);
    }

    protected void setQWForFiles(Writer writer)
    {
        qw = new CountingQuietWriter(writer, errorHandler);
    }

    // Taken verbatum from DailyRollingFileAppender
    int computeCheckPeriod()
    {
        RollingCalendar c = new RollingCalendar();
        // set sate to 1970-01-01 00:00:00 GMT
        Date epoch = new Date(0);
        if (datePattern != null)
        {
            for (int i = TOP_OF_MINUTE; i <= TOP_OF_MONTH; i++)
            {
                String r0 = sdf.format(epoch);
                c.setType(i);
                Date next = new Date(c.getNextCheckMillis(epoch));
                String r1 = sdf.format(next);
                // LogLog.debug("Type = "+i+", r0 = "+r0+", r1 = "+r1);
                if ((r0 != null) && (r1 != null) && !r0.equals(r1))
                {
                    return i;
                }
            }
        }

        return TOP_OF_TROUBLE; // Deliberately head for trouble...
    }

    // Now for the new stuff
    /**
     * Handles append time behavior for CompositeRollingAppender.  This checks if a roll over either by date (checked
     * first) or time (checked second) is need and then appends to the file last.
     */
    protected void subAppend(LoggingEvent event)
    {

        if (rollDate)
        {
            long n = System.currentTimeMillis();
            if (n >= nextCheck)
            {
                now.setTime(n);
                nextCheck = rc.getNextCheckMillis(now);

                rollOverTime();
            }
        }

        if (rollSize)
        {
            if ((fileName != null) && (((CountingQuietWriter) qw).getCount() >= maxFileSize))
            {
                rollOverSize();
            }
        }

        super.subAppend(event);
    }

    public void setFile(String file)
    {
        baseFileName = file.trim();
        fileName = file.trim();
    }

    /**
     * Creates and opens the file for logging.  If <code>staticLogFileName</code> is false then the fully qualified name
     * is determined and used.
     */
    public synchronized void setFile(String fileName, boolean append) throws IOException
    {
        if (!staticLogFileName)
        {
            scheduledFilename = fileName = fileName.trim() + sdf.format(now);
        }

        super.setFile(fileName, append, bufferedIO, bufferSize);

        if (append)
        {
            File f = new File(fileName);
            ((CountingQuietWriter) qw).setCount(f.length());
        }
    }

    public int getCountDirection()
    {
        return countDirection;
    }

    public void setCountDirection(int direction)
    {
        countDirection = direction;
    }

    public int getRollingStyle()
    {
        return rollingStyle;
    }

    public void setRollingStyle(int style)
    {
        rollingStyle = style;
        switch (rollingStyle)
        {

        case BY_SIZE:
            rollDate = false;
            rollSize = true;
            break;

        case BY_DATE:
            rollDate = true;
            rollSize = false;
            break;

        case BY_COMPOSITE:
            rollDate = true;
            rollSize = true;
            break;

        default:
            errorHandler.error("Invalid rolling Style, use 1 (by size only), 2 (by date only) or 3 (both)");
        }
    }

    /*
        public void setRollingStyle(String style) {
            if (style == S_BY_SIZE) {
                rollingStyle = BY_SIZE;
            }
            else if (style == S_BY_DATE) {
                rollingStyle = BY_DATE;
            }
            else if (style == S_BY_COMPOSITE) {
                rollingStyle = BY_COMPOSITE;
            }
        }
     */
    public boolean getStaticLogFileName()
    {
        return staticLogFileName;
    }

    public void setStaticLogFileName(boolean s)
    {
        staticLogFileName = s;
    }

    public void setStaticLogFileName(String value)
    {
        setStaticLogFileName(OptionConverter.toBoolean(value, true));
    }

    public boolean getCompressBackupFiles()
    {
        return compress;
    }

    public void setCompressBackupFiles(boolean c)
    {
        compress = c;
    }

    public boolean getCompressAsync()
    {
        return compressAsync;
    }

    public void setCompressAsync(boolean c)
    {
        compressAsync = c;
        if (compressAsync)
        {
            executor = Executors.newFixedThreadPool(1);

            compressor = new Compressor();
        }
    }

    public boolean getZeroBased()
    {
        return zeroBased;
    }

    public void setZeroBased(boolean z)
    {
        zeroBased = z;
    }

    public String getBackupFilesToPath()
    {
        return backupFilesToPath;
    }

    public void setbackupFilesToPath(String path)
    {
        File td = new File(path);
        if (!td.exists())
        {
            td.mkdirs();
        }

        backupFilesToPath = path;
    }

    /**
     * Initializes based on exisiting conditions at time of <code> activateOptions</code>.  The following is done:<br>
     * <br> A) determine curSizeRollBackups<br> B) determine curTimeRollBackups (not implemented)<br> C) initiates a
     * roll over if needed for crossing a date boundary since the last run.
     */
    protected void existingInit()
    {
        curTimeRollBackups = 0;

        // part A starts here
        // This is now down at first log when curSizeRollBackup==0 see rollFile
        // part A ends here

        // part B not yet implemented

        // part C
        if (staticLogFileName && rollDate)
        {
            File old = new File(baseFileName);
            if (old.exists())
            {
                Date last = new Date(old.lastModified());
                if (!(sdf.format(last).equals(sdf.format(now))))
                {
                    scheduledFilename = baseFileName + sdf.format(last);
                    LogLog.debug("Initial roll over to: " + scheduledFilename);
                    rollOverTime();
                }
            }
        }

        LogLog.debug("curSizeRollBackups after rollOver at: " + curSizeRollBackups);
        // part C ends here

    }

    /**
     * Sets initial conditions including date/time roll over information, first check, scheduledFilename, and calls
     * <code>existingInit</code> to initialize the current # of backups.
     */
    public void activateOptions()
    {

        // REMOVE removed rollDate from boolean to enable Alex's change
        if (datePattern != null)
        {
            now.setTime(System.currentTimeMillis());
            sdf = new SimpleDateFormat(datePattern);
            int type = computeCheckPeriod();
            // printPeriodicity(type);
            rc.setType(type);
            // next line added as this removes the name check in rollOver
            nextCheck = rc.getNextCheckMillis(now);
        }
        else
        {
            if (rollDate)
            {
                LogLog.error("Either DatePattern or rollingStyle options are not set for [" + name + "].");
            }
        }

        existingInit();

        if (rollDate && (fileName != null) && (scheduledFilename == null))
        {
            scheduledFilename = fileName + sdf.format(now);
        }

        try
        {
            this.setFile(fileName, true);
        }
        catch (IOException e)
        {
            errorHandler.error("Cannot set file name:" + fileName);
        }

        super.activateOptions();
    }

    /**
     * Rollover the file(s) to date/time tagged file(s). Opens the new file (through setFile) and resets
     * curSizeRollBackups.
     */
    protected void rollOverTime()
    {

        curTimeRollBackups++;

        this.closeFile(); // keep windows happy.


        rollFile();

        try
        {
            curSizeRollBackups = 0; // We're cleared out the old date and are ready for the new

            // new scheduled name
            scheduledFilename = fileName + sdf.format(now);
            this.setFile(baseFileName, false);
        }
        catch (IOException e)
        {
            errorHandler.error("setFile(" + fileName + ", false) call failed.");
        }

    }

    /**
     * Renames file <code>from</code> to file <code>to</code>.  It also checks for existence of target file and deletes
     * if it does.
     */
    protected void rollFile(String from, String to, boolean compress)
    {
        if (from.equals(to))
        {
            if (compress)
            {
                LogLog.error("Attempting to compress file with same output name.");
            }

            return;
        }

        File target = new File(to);

        File file = new File(from);
        // Perform Roll by renaming
        if (!file.getPath().equals(target.getPath()))
        {
            file.renameTo(target);
        }

        // Compress file after it has been moved out the way... this is safe
        // as it will gain a .gz ending and we can then safely delete this file
        // as it will not be the statically named value.
        if (compress)
        {
            compress(target);
        }

        LogLog.debug(from + " -> " + to);
    }

    private void compress(File target)
    {
        if (compressAsync)
        {
            synchronized (_compress)
            {
                _compress.offer(new CompressJob(target, target));
            }

            startCompression();
        }
        else
        {
            doCompress(target, target);
        }
    }

    private void startCompression()
    {
        if (_compressing.compareAndSet(false, true))
        {
            executor.execute(compressor);
        }
    }

    /** Delete's the specified file if it exists */
    protected void deleteFile(String fileName)
    {
        File file = compress ? new File(fileName + COMPRESS_EXTENSION) : new File(fileName);
        if (file.exists())
        {
            file.delete();
        }
    }

    /**
     * Implements roll overs base on file size.
     *
     * <p>If the maximum number of size based backups is reached (<code>curSizeRollBackups == maxSizeRollBackups</code)
     * then the oldest file is deleted -- it's index determined by the sign of countDirection.<br> If
     * <code>countDirection</code> < 0, then files {<code>File.1</code>, ..., <code>File.curSizeRollBackups -1</code>}
     * are renamed to {<code>File.2</code>, ..., <code>File.curSizeRollBackups</code>}.  Moreover, <code>File</code> is
     * renamed <code>File.1</code> and closed.<br>
     *
     * A new file is created to receive further log output.
     *
     * <p>If <code>maxSizeRollBackups</code> is equal to zero, then the <code>File</code> is truncated with no backup
     * files created.
     *
     * <p>If <code>maxSizeRollBackups</code> < 0, then <code>File</code> is renamed if needed and no files are deleted.
     */

    // synchronization not necessary since doAppend is alreasy synched
    protected void rollOverSize()
    {
        File file;

        this.closeFile(); // keep windows happy.

        LogLog.debug("rolling over count=" + ((CountingQuietWriter) qw).getCount());
        LogLog.debug("maxSizeRollBackups = " + maxSizeRollBackups);
        LogLog.debug("curSizeRollBackups = " + curSizeRollBackups);
        LogLog.debug("countDirection = " + countDirection);

        // If maxBackups <= 0, then there is no file renaming to be done.
        if (maxSizeRollBackups != 0)
        {
            rollFile();
        }

        try
        {
            // This will also close the file. This is OK since multiple
            // close operations are safe.
            this.setFile(baseFileName, false);
        }
        catch (IOException e)
        {
            LogLog.error("setFile(" + fileName + ", false) call failed.", e);
        }
    }

    /**
     * Perform file Rollover ensuring the countDirection is applied along with
     * the other options
     */
    private void rollFile()
    {
        LogLog.debug("CD="+countDirection+",start");
        if (countDirection < 0)
        {
            // If we haven't rolled yet then validate we have the right value
            // for curSizeRollBackups
            if (curSizeRollBackups == 0)
            {
                //Validate curSizeRollBackups
                curSizeRollBackups = countFileIndex(fileName);
                // decrement to offset the later increment
                curSizeRollBackups--;
            }

            // If we are not keeping an infinite set of backups the delete oldest
            if (maxSizeRollBackups > 0)
            {
                LogLog.debug("CD=-1,curSizeRollBackups:"+curSizeRollBackups);
                LogLog.debug("CD=-1,maxSizeRollBackups:"+maxSizeRollBackups);

                // Delete the oldest file.
                // curSizeRollBackups is never -1 so infinite backups are ok here
                if ((curSizeRollBackups - maxSizeRollBackups) >= 0)
                {
                    //The oldest file is the one with the largest number
                    // as the 0 is always fileName
                    // which moves to fileName.1 etc.
                    LogLog.debug("CD=-1,deleteFile:"+curSizeRollBackups);
                    deleteFile(fileName + '.' + curSizeRollBackups);
                    // decrement to offset the later increment
                    curSizeRollBackups--;
                }
            }
            // Map {(maxBackupIndex - 1), ..., 2, 1} to {maxBackupIndex, ..., 3, 2}
            for (int i = curSizeRollBackups; i >= 1; i--)
            {
                String oldName = (fileName + "." + i);
                String newName = (fileName + '.' + (i + 1));

                // Ensure that when compressing we rename the compressed archives
                if (compress)
                {
                    rollFile(oldName + COMPRESS_EXTENSION, newName + COMPRESS_EXTENSION, false);
                }
                else
                {
                    rollFile(oldName, newName, false);
                }
            }

            curSizeRollBackups++;
            // Rename fileName to fileName.1
            rollFile(fileName, fileName + ".1", compress);

        } // REMOVE This code branching for Alexander Cerna's request
        else if (countDirection == 0)
        {
            // rollFile based on date pattern
            now.setTime(System.currentTimeMillis());
            String newFile = fileName + sdf.format(now);
            
            // If we haven't rolled yet then validate we have the right value
            // for curSizeRollBackups
            if (curSizeRollBackups == 0)
            {
                //Validate curSizeRollBackups
                curSizeRollBackups = countFileIndex(newFile);
                // to balance the increment just coming up. as the count returns
                // the next free number not the last used.
                curSizeRollBackups--;
            }

            // If we are not keeping an infinite set of backups the delete oldest
            if (maxSizeRollBackups > 0)
            {
                // Don't prune older files if they exist just go for the last
                // one based on our maxSizeRollBackups. This means we may have
                // more files left on disk that maxSizeRollBackups if this value
                // is adjusted between runs but that is an acceptable state.
                // Otherwise we would have to check on startup that we didn't
                // have more than maxSizeRollBackups and prune then.

                if (((curSizeRollBackups - maxSizeRollBackups) >= 0))
                {
                    LogLog.debug("CD=0,curSizeRollBackups:"+curSizeRollBackups);
                    LogLog.debug("CD=0,maxSizeRollBackups:"+maxSizeRollBackups);

                    // delete the first and keep counting up.                                                                                               
                    int oldestFileIndex = curSizeRollBackups - maxSizeRollBackups + 1;
                    LogLog.debug("CD=0,deleteFile:"+oldestFileIndex);
                    deleteFile(newFile + '.' + oldestFileIndex);
                }
            }


            String finalName = newFile;

            curSizeRollBackups++;             

            // Add rollSize if it is > 0
            if (curSizeRollBackups > 0 ) 
            {
                finalName = newFile + '.' + curSizeRollBackups;

            }

            rollFile(fileName, finalName, compress);
        }
        else
        { // countDirection > 0
            // If we haven't rolled yet then validate we have the right value
            // for curSizeRollBackups
            if (curSizeRollBackups == 0)
            {
                //Validate curSizeRollBackups
                curSizeRollBackups = countFileIndex(fileName);
                // to balance the increment just coming up. as the count returns
                // the next free number not the last used.
                curSizeRollBackups--;
            }

            // If we are not keeping an infinite set of backups the delete oldest
            if (maxSizeRollBackups > 0)
            {
                LogLog.debug("CD=1,curSizeRollBackups:"+curSizeRollBackups);
                LogLog.debug("CD=1,maxSizeRollBackups:"+maxSizeRollBackups);

                // Don't prune older files if they exist just go for the last
                // one based on our maxSizeRollBackups. This means we may have
                // more files left on disk that maxSizeRollBackups if this value
                // is adjusted between runs but that is an acceptable state.
                // Otherwise we would have to check on startup that we didn't
                // have more than maxSizeRollBackups and prune then.

                if (((curSizeRollBackups - maxSizeRollBackups) >= 0))
                {
                    // delete the first and keep counting up.
                    int oldestFileIndex = curSizeRollBackups - maxSizeRollBackups + 1;
                    LogLog.debug("CD=1,deleteFile:"+oldestFileIndex);
                    deleteFile(fileName + '.' + oldestFileIndex);
                }
            }


            curSizeRollBackups++;

            rollFile(fileName, fileName + '.' + curSizeRollBackups, compress);

        }
        LogLog.debug("CD="+countDirection+",done");
    }


    private int countFileIndex(String fileName)
    {
        return countFileIndex(fileName, true);
    }
    /**
     * Use filename as a base name and find what count number we are up to by
     * looking at the files in this format:
     *
     * <filename>.<count>[COMPRESS_EXTENSION]
     *
     * If a count value of 1 cannot be found then a directory listing is
     * performed to try and identify if there is a valid value for <count>.
     * 
     *
     * @param fileName the basefilename to use
     * @param checkBackupLocation should backupFilesToPath location be checked for existing backups
     * @return int the next free index
     */
    private int countFileIndex(String fileName, boolean checkBackupLocation)
    {
        String testFileName;

        // It is possible for index 1..n to be missing leaving n+1..n+1+m logs
        // in this scenario we should still return n+1+m+1
        int index=1;

        testFileName = fileName + "." + index;

        // Bail out early if there is a problem with the file
        if (new File(testFileName) == null
                || new File(testFileName + COMPRESS_EXTENSION) == null)

        {
            return index;
        }

        // Check that we do not have the 1..n missing scenario
        if (!(new File(testFileName).exists()
              || new File(testFileName + COMPRESS_EXTENSION).exists()))

        {
            int max=0;
            String prunedFileName = new File(fileName).getName();

            // Look through all files to find next index
            if (new File(fileName).getParentFile() != null)
            {
                for (File file : new File(fileName).getParentFile().listFiles())
                {
                    String name = file.getName();

                    if (name.startsWith(prunedFileName) && !name.equals(prunedFileName))
                    {
                        String parsedCount = name.substring(prunedFileName.length() + 1);

                        if (parsedCount.endsWith(COMPRESS_EXTENSION))
                        {
                            parsedCount = parsedCount.substring(0, parsedCount.indexOf(COMPRESS_EXTENSION));
                        }

                        try
                        {
                            max = Integer.parseInt(parsedCount);

                            // if we got a good value then update our index value.
                            if (max > index)
                            {
                                // +1 as we want to return the next free value.
                                index = max + 1;
                            }
                        }
                        catch (NumberFormatException nfe)
                        {
                            //ignore it assume file doesn't exist.
                        }
                    }
                }
            }

            // Update testFileName 
            testFileName = fileName + "." + index;
        }


        while (new File(testFileName).exists()
                || new File(testFileName + COMPRESS_EXTENSION).exists())
        {
            index++;
            testFileName = fileName + "." + index;
        }

        if (checkBackupLocation && index == 1 && backupFilesToPath != null)
        {
            LogLog.debug("Trying backup location:"+backupFilesToPath + System.getProperty("file.separator") + fileName);
            return countFileIndex(backupFilesToPath + System.getProperty("file.separator") + new File(fileName).getName(), false);
        }

        return index;
    }

    protected synchronized void doCompress(File from, File to)
    {
        String toFile;
        if (backupFilesToPath == null)
        {
            toFile = to.getPath() + COMPRESS_EXTENSION;
        }
        else
        {
            toFile = backupFilesToPath + System.getProperty("file.separator") + to.getName() + COMPRESS_EXTENSION;
        }

        File target = new File(toFile);
        if (target.exists())
        {
            LogLog.debug("deleting existing target file: " + target);
            target.delete();
        }

        try
        {
            // Create the GZIP output stream
            GZIPOutputStream out = new GZIPOutputStream(new FileOutputStream(target));

            // Open the input file
            FileInputStream in = new FileInputStream(from);

            // Transfer bytes from the input file to the GZIP output stream
            byte[] buf = new byte[1024];
            int len;
            while ((len = in.read(buf)) > 0)
            {
                out.write(buf, 0, len);
            }

            in.close();

            // Complete the GZIP file
            out.finish();
            out.close();
            // Remove old file.
            from.delete();
        }
        catch (IOException e)
        {
            if (target.exists())
            {
                target.delete();
            }

            rollFile(from.getPath(), to.getPath(), false);
        }
    }

    private class CompressJob
    {
        File _from, _to;

        CompressJob(File from, File to)
        {
            _from = from;
            _to = to;
        }

        File getFrom()
        {
            return _from;
        }

        File getTo()
        {
            return _to;
        }
    }

    Compressor compressor = null;

    Executor executor;

    private class Compressor implements Runnable
    {
        public void run()
        {
            boolean running = true;
            while (running)
            {
                CompressJob job = _compress.poll();

                doCompress(job.getFrom(), job.getTo());

                synchronized (_compress)
                {
                    if (_compress.isEmpty())
                    {
                        running = false;
                        _compressing.set(false);
                    }
                }
            }

        }
    }
}
