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
package org.apache.qpid.util;

import junit.framework.TestCase;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileWriter;
import java.io.IOException;
import java.util.List;

public class FileUtilsTest extends TestCase
{
    private static final String COPY = "-Copy";
    private static final String SUB = "-Sub";

    /**
     * Additional test for the copy method.
     * Ensures that the directory count did increase by more than 1 after the copy.
     */
    public void testCopyFile()
    {
        final String TEST_DATA = "FileUtilsTest-testCopy-TestDataTestDataTestDataTestDataTestDataTestData";
        String fileName = "FileUtilsTest-testCopy";
        String fileNameCopy = fileName + COPY;

        File[] beforeCopyFileList = null;

        //Create initial file
        File test = createTestFile(fileName, TEST_DATA);

        try
        {
            //Check number of files before copy
            beforeCopyFileList = test.getAbsoluteFile().getParentFile().listFiles();
            int beforeCopy = beforeCopyFileList.length;

            //Perform Copy
            File destination = new File(fileNameCopy);
            FileUtils.copy(test, destination);
            //Ensure the JVM cleans up if cleanup failues
            destination.deleteOnExit();

            //Retrieve counts after copy
            int afterCopy = test.getAbsoluteFile().getParentFile().listFiles().length;

            int afterCopyFromCopy = new File(fileNameCopy).getAbsoluteFile().getParentFile().listFiles().length;

            // Validate the copy counts
            assertEquals("The file listing from the original and the copy differ in length.", afterCopy, afterCopyFromCopy);
            assertEquals("The number of files did not increase.", beforeCopy + 1, afterCopy);
            assertEquals("The number of files did not increase.", beforeCopy + 1, afterCopyFromCopy);

            //Validate copy
            // Load content
            String copiedFileContent = FileUtils.readFileAsString(fileNameCopy);
            assertEquals(TEST_DATA, copiedFileContent);
        }
        finally // Ensure clean
        {
            //Clean up
            assertTrue("Unable to cleanup", FileUtils.deleteFile(fileNameCopy));

            //Check file list after cleanup
            File[] afterCleanup = new File(test.getAbsoluteFile().getParent()).listFiles();
            checkFileLists(beforeCopyFileList, afterCleanup);

            //Remove original file
            assertTrue("Unable to cleanup", test.delete());
        }
    }

    /**
     * Create and Copy the following structure:
     *
     * testDirectory --+
     * +-- testSubDirectory --+
     * +-- testSubFile
     * +-- File
     *
     * to testDirectory-Copy
     *
     * Validate that the file count in the copy is correct and contents of the copied files is correct.
     */
    public void testCopyRecursive()
    {
        final String TEST_DATA = "FileUtilsTest-testDirectoryCopy-TestDataTestDataTestDataTestDataTestDataTestData";
        String fileName = "FileUtilsTest-testCopy";
        String TEST_DIR = "testDirectoryCopy";

        //Create Initial Structure
        File testDir = new File(TEST_DIR);

        //Check number of files before copy
        File[] beforeCopyFileList = testDir.getAbsoluteFile().getParentFile().listFiles();

        try
        {
            //Create Directories
            assertTrue("Test directory already exists cannot test.", !testDir.exists());

            if (!testDir.mkdir())
            {
                fail("Unable to make test Directory");
            }

            File testSubDir = new File(TEST_DIR + File.separator + TEST_DIR + SUB);
            if (!testSubDir.mkdir())
            {
                fail("Unable to make test sub Directory");
            }

            //Create Files
            createTestFile(testDir.toString() + File.separator + fileName, TEST_DATA);
            createTestFile(testSubDir.toString() + File.separator + fileName + SUB, TEST_DATA);

            //Ensure the JVM cleans up if cleanup failues
            testSubDir.deleteOnExit();
            testDir.deleteOnExit();

            //Perform Copy
            File copyDir = new File(testDir.toString() + COPY);
            try
            {
                FileUtils.copyRecursive(testDir, copyDir);
            }
            catch (FileNotFoundException e)
            {
                fail(e.getMessage());
            }
            catch (FileUtils.UnableToCopyException e)
            {
                fail(e.getMessage());
            }

            //Validate Copy
            assertEquals("Copied directory should only have one file and one directory in it.", 2, copyDir.listFiles().length);

            //Validate Copy File Contents
            String copiedFileContent = FileUtils.readFileAsString(copyDir.toString() + File.separator + fileName);
            assertEquals(TEST_DATA, copiedFileContent);

            //Validate Name of Sub Directory
            assertTrue("Expected subdirectory is not a directory", new File(copyDir.toString() + File.separator + TEST_DIR + SUB).isDirectory());

            //Assert that it contains only one item
            assertEquals("Copied sub directory should only have one directory in it.", 1, new File(copyDir.toString() + File.separator + TEST_DIR + SUB).listFiles().length);

            //Validate content of Sub file
            copiedFileContent = FileUtils.readFileAsString(copyDir.toString() + File.separator + TEST_DIR + SUB + File.separator + fileName + SUB);
            assertEquals(TEST_DATA, copiedFileContent);
        }
        finally
        {
            //Clean up source and copy directory.
            assertTrue("Unable to cleanup", FileUtils.delete(testDir, true));
            assertTrue("Unable to cleanup", FileUtils.delete(new File(TEST_DIR + COPY), true));

            //Check file list after cleanup
            File[] afterCleanup = testDir.getAbsoluteFile().getParentFile().listFiles();
            checkFileLists(beforeCopyFileList, afterCleanup);
        }
    }

    /**
     * Helper method to create a test file with a string content
     *
     * @param fileName  The fileName to use in the creation
     * @param test_data The data to store in the file
     *
     * @return The File reference
     */
    private File createTestFile(String fileName, String test_data)
    {
        File test = new File(fileName);

        try
        {
            test.createNewFile();
            //Ensure the JVM cleans up if cleanup failues
            test.deleteOnExit();
        }
        catch (IOException e)
        {
            fail(e.getMessage());
        }

        BufferedWriter writer = null;
        try
        {
            writer = new BufferedWriter(new FileWriter(test));
            try
            {
                writer.write(test_data);
            }
            catch (IOException e)
            {
                fail(e.getMessage());
            }
        }
        catch (IOException e)
        {
            fail(e.getMessage());
        }
        finally
        {
            try
            {
                if (writer != null)
                {
                    writer.close();
                }
            }
            catch (IOException e)
            {
                fail(e.getMessage());
            }
        }

        return test;
    }

    /** Test that deleteFile only deletes the specified file */
    public void testDeleteFile()
    {
        File test = new File("FileUtilsTest-testDelete");
        //Record file count in parent directory to check it is not changed by delete
        String path = test.getAbsolutePath();
        File[] filesBefore = new File(path.substring(0, path.lastIndexOf(File.separator))).listFiles();
        int fileCountBefore = filesBefore.length;

        try
        {
            test.createNewFile();
            //Ensure the JVM cleans up if cleanup failues
            test.deleteOnExit();
        }
        catch (IOException e)
        {
            fail(e.getMessage());
        }

        assertTrue("File does not exists", test.exists());
        assertTrue("File is not a file", test.isFile());

        //Check that file creation can be seen on disk
        int fileCountCreated = new File(path.substring(0, path.lastIndexOf(File.separator))).listFiles().length;
        assertEquals("File creation was no registered", fileCountBefore + 1, fileCountCreated);

        //Perform Delete
        assertTrue("Unable to cleanup", FileUtils.deleteFile("FileUtilsTest-testDelete"));

        assertTrue("File exists after delete", !test.exists());

        //Check that after deletion the file count is now accurate
        File[] filesAfter = new File(path.substring(0, path.lastIndexOf(File.separator))).listFiles();
        int fileCountAfter = filesAfter.length;
        assertEquals("File creation was no registered", fileCountBefore, fileCountAfter);

        checkFileLists(filesBefore, filesAfter);
    }

    public void testDeleteNonExistentFile()
    {
        File test = new File("FileUtilsTest-testDelete-" + System.currentTimeMillis());

        assertTrue("File exists", !test.exists());
        assertFalse("File is a directory", test.isDirectory());

        assertTrue("Delete Succeeded ", !FileUtils.delete(test, true));
    }

    public void testDeleteNull()
    {
        try
        {
            FileUtils.delete(null, true);
            fail("Delete with null value should throw NPE.");
        }
        catch (NullPointerException npe)
        {
            // expected path
        }
    }

    /**
     * Given two lists of File arrays ensure they are the same length and all entries in Before are in After
     *
     * @param filesBefore File[]
     * @param filesAfter  File[]
     */
    private void checkFileLists(File[] filesBefore, File[] filesAfter)
    {
        assertNotNull("Before file list cannot be null", filesBefore);
        assertNotNull("After file list cannot be null", filesAfter);

        assertEquals("File lists are unequal", filesBefore.length, filesAfter.length);

        for (File fileBefore : filesBefore)
        {
            boolean found = false;

            for (File fileAfter : filesAfter)
            {
                if (fileBefore.getAbsolutePath().equals(fileAfter.getAbsolutePath()))
                {
                    found = true;
                    break;
                }
            }

            assertTrue("File'" + fileBefore.getName() + "' was not in directory afterwards", found);
        }
    }

    public void testNonRecursiveNonEmptyDirectoryDeleteFails()
    {
        String directoryName = "FileUtilsTest-testRecursiveDelete";
        File test = new File(directoryName);

        //Record file count in parent directory to check it is not changed by delete
        String path = test.getAbsolutePath();
        File[] filesBefore = new File(path.substring(0, path.lastIndexOf(File.separator))).listFiles();
        int fileCountBefore = filesBefore.length;

        assertTrue("Directory exists", !test.exists());

        test.mkdir();

        //Create a file in the directory
        String fileName = test.getAbsolutePath() + File.separatorChar + "testFile";
        File subFile = new File(fileName);
        try
        {
            subFile.createNewFile();
            //Ensure the JVM cleans up if cleanup failues
            subFile.deleteOnExit();
        }
        catch (IOException e)
        {
            fail(e.getMessage());
        }
        //Ensure the JVM cleans up if cleanup failues
        // This must be after the subFile as the directory must be empty before
        // the delete is performed
        test.deleteOnExit();

        //Try and delete the non-empty directory
        assertFalse("Non Empty Directory was successfully deleted.", FileUtils.deleteDirectory(directoryName));

        //Check directory is still there
        assertTrue("Directory was deleted.", test.exists());

        // Clean up
        assertTrue("Unable to cleanup", FileUtils.delete(test, true));

        //Check that after deletion the file count is now accurate
        File[] filesAfter = new File(path.substring(0, path.lastIndexOf(File.separator))).listFiles();
        int fileCountAfter = filesAfter.length;
        assertEquals("File creation was no registered", fileCountBefore, fileCountAfter);

        checkFileLists(filesBefore, filesAfter);
    }

    /** Test that an empty directory can be deleted with deleteDirectory */
    public void testEmptyDirectoryDelete()
    {
        String directoryName = "FileUtilsTest-testRecursiveDelete";
        File test = new File(directoryName);

        //Record file count in parent directory to check it is not changed by delete
        String path = test.getAbsolutePath();
        File[] filesBefore = new File(path.substring(0, path.lastIndexOf(File.separator))).listFiles();
        int fileCountBefore = filesBefore.length;

        assertTrue("Directory exists", !test.exists());

        test.mkdir();
        //Ensure the JVM cleans up if cleanup failues
        test.deleteOnExit();

        //Try and delete the empty directory
        assertTrue("Non Empty Directory was successfully deleted.", FileUtils.deleteDirectory(directoryName));

        //Check directory is still there
        assertTrue("Directory was deleted.", !test.exists());

        //Check that after deletion the file count is now accurate
        File[] filesAfter = new File(path.substring(0, path.lastIndexOf(File.separator))).listFiles();
        int fileCountAfter = filesAfter.length;
        assertEquals("File creation was no registered", fileCountBefore, fileCountAfter);

        checkFileLists(filesBefore, filesAfter);

    }

    /** Test that deleteDirectory on a non empty directory to complete */
    public void testNonEmptyDirectoryDelete()
    {
        String directoryName = "FileUtilsTest-testRecursiveDelete";
        File test = new File(directoryName);

        assertTrue("Directory exists", !test.exists());

        //Record file count in parent directory to check it is not changed by delete
        String path = test.getAbsolutePath();
        File[] filesBefore = new File(path.substring(0, path.lastIndexOf(File.separator))).listFiles();
        int fileCountBefore = filesBefore.length;

        test.mkdir();

        //Create a file in the directory
        String fileName = test.getAbsolutePath() + File.separatorChar + "testFile";
        File subFile = new File(fileName);
        try
        {
            subFile.createNewFile();
            //Ensure the JVM cleans up if cleanup failues
            subFile.deleteOnExit();
        }
        catch (IOException e)
        {
            fail(e.getMessage());
        }

        // Ensure the JVM cleans up if cleanup failues
        // This must be after the subFile as the directory must be empty before
        // the delete is performed
        test.deleteOnExit();

        //Try and delete the non-empty directory non-recursively
        assertFalse("Non Empty Directory was successfully deleted.", FileUtils.delete(test, false));

        //Check directory is still there
        assertTrue("Directory was deleted.", test.exists());

        // Clean up
        assertTrue("Unable to cleanup", FileUtils.delete(test, true));

        //Check that after deletion the file count is now accurate
        File[] filesAfter = new File(path.substring(0, path.lastIndexOf(File.separator))).listFiles();
        int fileCountAfter = filesAfter.length;
        assertEquals("File creation was no registered", fileCountBefore, fileCountAfter);

        checkFileLists(filesBefore, filesAfter);

    }

    /** Test that a recursive delete successeds */
    public void testRecursiveDelete()
    {
        String directoryName = "FileUtilsTest-testRecursiveDelete";
        File test = new File(directoryName);

        assertTrue("Directory exists", !test.exists());

        //Record file count in parent directory to check it is not changed by delete
        String path = test.getAbsolutePath();
        File[] filesBefore = new File(path.substring(0, path.lastIndexOf(File.separator))).listFiles();
        int fileCountBefore = filesBefore.length;

        test.mkdir();

        createSubDir(directoryName, 2, 4);

        //Ensure the JVM cleans up if cleanup failues
        // This must be after the sub dir creation as the delete order is
        // recorded and the directory must be empty to be deleted.
        test.deleteOnExit();

        assertFalse("Non recursive delete was able to directory", FileUtils.delete(test, false));

        assertTrue("File does not exist after non recursive delete", test.exists());

        assertTrue("Unable to cleanup", FileUtils.delete(test, true));

        assertTrue("File  exist after recursive delete", !test.exists());

        //Check that after deletion the file count is now accurate
        File[] filesAfter = new File(path.substring(0, path.lastIndexOf(File.separator))).listFiles();
        int fileCountAfter = filesAfter.length;
        assertEquals("File creation was no registered", fileCountBefore, fileCountAfter);

        checkFileLists(filesBefore, filesAfter);

    }

    private void createSubDir(String path, int directories, int files)
    {
        File directory = new File(path);

        assertTrue("Directory" + path + " does not exists", directory.exists());

        for (int dir = 0; dir < directories; dir++)
        {
            String subDirName = path + File.separatorChar + "sub" + dir;
            File subDir = new File(subDirName);

            subDir.mkdir();

            createSubDir(subDirName, directories - 1, files);
            //Ensure the JVM cleans up if cleanup failues
            // This must be after the sub dir creation as the delete order is
            // recorded and the directory must be empty to be deleted.
            subDir.deleteOnExit();
        }

        for (int file = 0; file < files; file++)
        {
            String subDirName = path + File.separatorChar + "file" + file;
            File subFile = new File(subDirName);
            try
            {
                subFile.createNewFile();
                //Ensure the JVM cleans up if cleanup failues
                subFile.deleteOnExit();
            }
            catch (IOException e)
            {
                fail(e.getMessage());
            }
        }
    }

    public static final String SEARCH_STRING = "testSearch";

    /**
     * Test searchFile(File file, String search) will find a match when it
     * exists.
     *
     * @throws java.io.IOException if unable to perform test setup
     */
    public void testSearchSucceed() throws IOException
    {
        File _logfile = File.createTempFile("FileUtilsTest-testSearchSucceed", ".out");

        prepareFileForSearchTest(_logfile);

        List<String> results = FileUtils.searchFile(_logfile, SEARCH_STRING);

        assertNotNull("Null result set returned", results);

        assertEquals("Results do not contain expected count", 1, results.size());
    }

    /**
     * Test searchFile(File file, String search) will not find a match when the
     * test string does not exist.
     *
     * @throws java.io.IOException if unable to perform test setup
     */
    public void testSearchFail() throws IOException
    {
        File _logfile = File.createTempFile("FileUtilsTest-testSearchFail", ".out");

        prepareFileForSearchTest(_logfile);

        List<String> results = FileUtils.searchFile(_logfile, "Hello");

        assertNotNull("Null result set returned", results);

        //Validate we only got one message
        if (results.size() > 0)
        {
            System.err.println("Unexpected messages");

            for (String msg : results)
            {
                System.err.println(msg);
            }
        }

        assertEquals("Results contains data when it was not expected",
                     0, results.size());
    }

    /**
     * Write the SEARCH_STRING in to the given file.
     *
     * @param logfile The file to write the SEARCH_STRING into
     *
     * @throws IOException if an error occurs
     */
    private void prepareFileForSearchTest(File logfile) throws IOException
    {
        BufferedWriter writer = new BufferedWriter(new FileWriter(logfile));
        writer.append(SEARCH_STRING);
        writer.flush();
        writer.close();
    }

}
