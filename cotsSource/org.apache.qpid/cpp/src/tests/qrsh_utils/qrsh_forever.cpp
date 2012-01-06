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


#include <stdio.h>
#include <unistd.h>



main ( int argc, char ** argv )
{
    fprintf ( stderr, "Hello, I am the Forever Example Child!\n");
    fprintf ( stderr, "my %d arguments are:\n", argc - 1 );

    int i;
    for ( i = 1; i < argc; ++ i )
        fprintf ( stderr, "arg %d: |%s|\n", i, argv[i] );

    for ( i = 0; i >= 0; ++ i )
    {
        fprintf ( stderr, "child sleeping forever %d ...\n" , i);
        sleep ( 1 );
    }

    fprintf ( stderr, "child exiting with code 12.\n" );

    return 12;
}




