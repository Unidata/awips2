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
package org.apache.qpid.example.publisher;

import java.io.File;

import javax.jms.JMSException;


import org.apache.qpid.example.shared.FileUtils;
import org.apache.qpid.example.shared.Statics;
import org.slf4j.LoggerFactory;
import org.slf4j.Logger;

/**
 * Class that sends message files to the Publisher to distribute
 * using files as input
 * Must set properties for host in properties file or uses in vm broker
 */
public class FileMessageDispatcher
{

    protected static final Logger _logger = LoggerFactory.getLogger(FileMessageDispatcher.class);

    protected static Publisher _publisher = null;

    /**
     * To use this main method you need to specify a path or file to use for input
     * This class then uses file contents from the dir/file specified to generate
     * messages to publish
     * Intended to be a very simple way to get going with publishing using the broker
     * @param args - must specify one value, the path to file(s) for publisher
     */
    public static void main(String[] args)
    {

        // Check command line args ok - must provide a path or file for us to dispatch
        if (args.length == 0)
        {
            System.out.println("Usage: FileMessageDispatcher <filesToDispatch>" + "");
        }
        else
        {
            try
            {
                // publish message(s) from file(s) to configured queue
                publish(args[0]);

                // Move payload file(s) to archive location as no error
                FileUtils.moveFileToNewDir(args[0], System.getProperties().getProperty(Statics.ARCHIVE_PATH));
            }
            catch (Exception e)
            {
                // log error and exit
                _logger.error("Error trying to dispatch message: " + e);
                System.exit(1);
            }
            finally
            {
                // clean up before exiting
                if (getPublisher() != null)
                {
                    getPublisher().cleanup();
                }
            }
        }

        if (_logger.isDebugEnabled())
        {
            _logger.debug("Finished dispatching message");
        }

        System.exit(0);
    }

    /**
     * Publish the content of a file or files from a directory as messages
     * @param path - from main args
     * @throws JMSException
     * @throws MessageFactoryException - if cannot create message from file content
     */
    public static void publish(String path) throws JMSException, MessageFactoryException
    {
        File tempFile = new File(path);
        if (tempFile.isDirectory())
        {
            // while more files in dir publish them
            File[] files = tempFile.listFiles();

            if ((files == null) || (files.length == 0))
            {
                _logger.info("FileMessageDispatcher - No files to publish in input directory: " + tempFile);
            }
            else
            {
                for (File file : files)
                {
                    // Create message factory passing in payload path
                    FileMessageFactory factory = new FileMessageFactory(getPublisher().getSession(), file.toString());

                    // Send the message generated from the payload using the _publisher
                    getPublisher().sendMessage(factory.createEventMessage());

                }
            }
        }
        else
        {
            // handle a single file
            // Create message factory passing in payload path
            FileMessageFactory factory = new FileMessageFactory(getPublisher().getSession(), tempFile.toString());

            // Send the message generated from the payload using the _publisher
            getPublisher().sendMessage(factory.createEventMessage());
        }
    }

    /**
     * Cleanup before exit
     */
    public static void cleanup()
    {
        if (getPublisher() != null)
        {
            getPublisher().cleanup();
        }
    }

    /**
     * @return A Publisher instance
     */
    private static Publisher getPublisher()
    {
        if (_publisher != null)
        {
            return _publisher;
        }

        // Create a _publisher
        _publisher = new Publisher();

        return _publisher;
    }

}
