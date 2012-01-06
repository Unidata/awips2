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

#include <iostream>
#include <sstream>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <fcntl.h>
#include <errno.h>

#include <sys/stat.h>
#include <sys/types.h>
#include <sys/wait.h>


using namespace std;



int 
main ( int argc, char ** argv )
{
    int  exit_code = -1;
    int  fd[2];
    int  my_pid = getpid();
    int  child_pid;

    pipe(fd); 

    char const * root_dir   = argv[1]; // This arg is prepended by qrsh_server.
    char const * child_name = argv[2]; // This arg comes from qrsh.
    char const * child_path = argv[3]; // This arg comes from qrsh.

    // This is the problem..
    fprintf ( stderr, "MDEBUG  qrsh_run: root_dir: |%s|\n", root_dir );
    fprintf ( stderr, "MDEBUG  qrsh_run: child_name: |%s|\n", child_name );
    fprintf ( stderr, "MDEBUG  qrsh_run: child_path: |%s|\n", child_path );

    /*
     * A named child is one for whom we will create a directory and
     * store information.  There are some magic names that are not
     * real symbolic names -- but are instead the names of actions.
     */

    bool named_child = true;

    if ( ! strcmp ( child_name, "exec" ) )
        named_child = false;
    else
    if ( ! strcmp ( child_name, "exec_wait" ) )
        named_child = false;
    else
    if ( ! strcmp ( child_name, "exited" ) )
        named_child = false;
    else
        named_child = true;

    stringstream child_dir_name;

    if ( named_child ) 
    {
      child_dir_name << root_dir
                     << '/'
                     << child_name;

      /*
       *  Make the child directory before forking, or there is
       *  a race in which the child might be trying to make its 
       *  stdout and stderr files while we are tring to make 
       *  the directory.
       */
      if ( -1 == mkdir ( child_dir_name.str().c_str(), 0777 ) )
      {
        fprintf ( stderr, 
                  "qrsh_run error: Can't mkdir |%s|\n", 
                  child_dir_name.str().c_str() 
                );
        exit ( 1 );
      }

    }
    else
    /*
     * If this is an 'exited' command that means we are 
     * waiting for a pre-existing child.
     */
    if ( ! strcmp ( child_name, "exited" ) )
    {
      int wait_pid = atoi(child_path);
  
      // Find the child's symbolic name.
      stringstream pid_to_name_file_name;
      pid_to_name_file_name << root_dir
                            << '/'
                            << wait_pid;
      FILE * fp = fopen ( pid_to_name_file_name.str().c_str(), "r" );
      if (! fp)
      {
          fprintf ( stderr, 
                    "qrsh_run %d error: Can't open pid2name file |%s|.\n",
                    my_pid,
                    pid_to_name_file_name.str().c_str()
                  );
          exit(1);
      }
      char symbolic_name[1000];
      strcpy ( symbolic_name, "qrsh_no_name" );
      fscanf ( fp, "%s", symbolic_name );
      fclose ( fp );

      // Make the name of the child's exit code file.
      stringstream exit_code_file_name;
      exit_code_file_name << root_dir
                          << '/'
                          << symbolic_name
                          << "/exit_code";

      struct stat stat_buf;
      int file_does_not_exist = stat ( exit_code_file_name.str().c_str(), & stat_buf );

      /*
       * If the result of stat is zero, the file exists, which means that
       * the command has exited.  The question we are being asked here is
       * "has it exited yet?"  
       */
      if ( ! file_does_not_exist )
          return 1;
      else
      if ( errno == ENOENT )
         return 0;
      else
         return 2 ;
    }


    // We are not waiting on a pre-wxiting child: we have a 
    // new child to create.
  
    child_pid = fork();

    if ( child_pid == 0 ) 
    {
        // This code is executed in the child process.

        // If it's a *named* child, then redirect its stdout and stderr.
        if ( named_child )
        {
            stringstream stdout_path,
                         stderr_path;

            // Redirect the child's stdout. -----------------
            stdout_path << root_dir
                        << '/'
                        << child_name
                        << '/'
                        << "stdout";
      
            int redirected_stdout = open ( stdout_path.str().c_str(),
                                           O_WRONLY|O_CREAT|O_TRUNC,
                                           S_IRWXU|S_IRWXG|S_IRWXO
                                         );
            if ( redirected_stdout < 0 )
            {
                perror ( "qrsh_run: error opening redirected_stdout: " );
                fprintf ( stderr, "stdout path: |%s|\n", stdout_path.str().c_str() );
                exit ( 1 );
            }
            if ( -1 == dup2 ( redirected_stdout, 1 ) )
            {
                perror ( "qrsh_run: dup2 (stdout) error: " );
                exit(1);
            }

            // Redirect the child's stderr. -----------------
            stderr_path << root_dir
                        << '/'
                        << child_name
                        << '/'
                        << "stderr";
      
            int redirected_stderr = open ( stderr_path.str().c_str(),
                                           O_WRONLY|O_CREAT|O_TRUNC,
                                           S_IRWXU|S_IRWXG|S_IRWXO
                                         );
            if ( redirected_stderr < 0 )
            {
                perror ( "qrsh_run: error opening redirected_stderr: " );
                fprintf ( stderr, "stderr path: |%s|\n", stderr_path.str().c_str() );
                exit ( 1 );
            }
            if(-1 == dup2 ( redirected_stderr, 2 ) )
            {
                perror ( "qrsh_run: dup2 (stderr) error: " );
                exit(1);
            }
        }

        fprintf ( stderr, "MDEBUG  -------------  qrsh_run argv   -------------\n" );
        for ( int i = 0; i < argc; ++ i )
            fprintf ( stderr, "MDEBUG  argv[%d] : |%s|\n", i, argv[i] );

        execv ( child_path, argv + 2 );
        perror ( "qrsh_run: execv error: " );
        fprintf ( stderr, "on path |%s|\n", child_path );
        exit ( 1 );
    }
    else
    {
        // This code is executed in the parent process.

        if ( named_child )
        {
            // Write the name-to-pid mapping.
            stringstream pid_file_name;
            pid_file_name << child_dir_name.str()
                          << "/pid";
      
            FILE * fp;
            if ( ! (fp = fopen ( pid_file_name.str().c_str(), "w") ) )
            {
                fprintf ( stderr, 
                          "qrsh_run %d error: Can't open file |%s|\n", 
                          my_pid, 
                          pid_file_name.str().c_str() 
                        );
                exit(1);
            }
            fprintf ( fp, "%d\n", child_pid );
            fclose ( fp );


            // Write the pid-to-name mapping.
            stringstream name_to_pid_file_name;
            name_to_pid_file_name << root_dir
                                  << '/'
                                  << child_pid;
            if(! (fp = fopen ( name_to_pid_file_name.str().c_str(), "w")))
            {
                fprintf ( stderr, 
                          "qrsh_run %d error: Can't open file |%s|\n", 
                          my_pid, 
                          name_to_pid_file_name.str().c_str() 
                        );
                exit(1);
            }
            fprintf ( fp, "%s\n", child_name );
            fclose(fp);
        }

        pid_t awaited_pid;
        while ( 0 == (awaited_pid = waitpid ( child_pid, & exit_code, WNOHANG)) )
        {
            fprintf ( stderr, 
                      "qrsh_run %d info: parent: waiting for child %d...\n",
                      my_pid,
                      child_pid
                    );
            sleep(1);
        }

        if ( -1 == awaited_pid )
        {
            fprintf ( stderr, "qrsh_run error awaiting child!\n" );
            exit ( 1 );
        }
  
        /*
         * Write the exit code.
         */
        exit_code >>= 8;

        if ( named_child )
        {
            if ( child_pid == awaited_pid )
            {
                stringstream exit_code_file_name;
                exit_code_file_name << child_dir_name.str()
                                    << "/exit_code";
        
                FILE * fp;
                if ( ! (fp = fopen ( exit_code_file_name.str().c_str(), "w") ) )
                {
                    fprintf ( stderr, 
                              "qrsh_run error: Can't open file |%s|\n", 
                              exit_code_file_name.str().c_str() 
                            );
                    exit(1);
                }
                fprintf ( fp, "%d\n", exit_code );
                fclose ( fp );
            }
        }
    }

    fprintf ( stderr, "MDEBUG  qrsh_run returning exit code %d\n", exit_code );
    return exit_code;
}




