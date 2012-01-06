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
#include <cstdlib>
#include <iostream>
#include <map>
#include <dirent.h>

#include <sys/stat.h>
#include <sys/types.h>
#include <sys/wait.h>

#include <qpid/client/Connection.h>
#include <qpid/client/Session.h>
#include <qpid/client/AsyncSession.h>
#include <qpid/client/Message.h>
#include <qpid/client/MessageListener.h>
#include <qpid/client/SubscriptionManager.h>


using namespace qpid::client;
using namespace qpid::framing;
using namespace std;


namespace qpid {
namespace tests {

int
mrand ( int max_desired_val )
{
  double zero_to_one = (double) rand() / (double) RAND_MAX;
  return (int) (zero_to_one * (double) max_desired_val);
}



char *
file2str ( char const * file_name )
{
  FILE * fp = fopen ( file_name, "r" );
  if(! fp)
  {
    fprintf ( stderr, "file2str error: can't open file |%s|.\n", file_name );
    return 0;
  }

  fseek ( fp, 0, SEEK_END );
  size_t file_len = (size_t) ftell ( fp );
  rewind ( fp );
  char * content = (char *) malloc ( file_len + 1 );

  if ( ! content )
  {
    fprintf ( stderr,
              "file2str error: can't malloc %d bytes.\n",
              (int)file_len
            );
    return 0;
  }

  size_t items_read = fread ( content, file_len, 1, fp );

  if ( 1 != items_read )
  {
    fprintf ( stderr, "file2str error: read failed.\n" );
    free ( content );
    return 0;
  }

  fclose ( fp );
  content[file_len] = 0;

  return content;
}





class QrshServer : public MessageListener
{
  public:

    QrshServer ( SubscriptionManager & subscriptions,
                char const * name,
                char const * qrsh_run_path,
                char const * host,
                int          port
              );

    virtual void received ( Message & message);


  private:

    set<string> all_server_names;

    stringstream data_dir;

    SubscriptionManager & subscriptions;

    // Is this message addressed to me?
    bool myMessage            ( Message const & message );

    /* ----------------------------------------------
     * Special Commands
     * These are commands that the qrsh_server executes
     * directly, rather than through a child process
     * instance of qrsh_run.
     */
    void runCommand           ( Message const & message );
    void execute              ( Message const & message );
    void wait                 ( Message const & message );
    void exited               ( Message const & message );
    void get                  ( Message const & message );
    void rememberIntroduction ( Message const & message );
    void getStraw             ( Message const & message );
    void addAlias             ( Message const & message );

    void start    ( );
    void sayHello ( );
    void sayName  ( );
    // end Special Commands ------------------------


    void saveCommand          ( Message const & message );

    void send                 ( string const & content );

    void drawStraws      ( );
    void getNames        ( );
    void runSavedCommand ( );

    char ** getArgs        ( char const * s );
    bool isProcessName     ( char const * s );
    int  string_countWords ( char const * s );
    char const * skipWord  ( char const * s );


    void string_replaceAll ( string & str,
                             string & target,
                             string & replacement
                           );


    string name,
           qrsh_run_path,
           host;

    vector<string *> aliases;

    int port;

    map < char *, int > abstract_name_map;

    set < string > myFellowBrokers;

    bool saidHello;

    Message savedCommand;

    vector < int > straws;
    int myStraw;

};



QrshServer::QrshServer ( SubscriptionManager & subs,
                         char const * name,
                         char const * qrsh_run_path,
                         char const * host,
                         int          port
                       )
    : subscriptions ( subs ),
      name ( name ),
      qrsh_run_path ( qrsh_run_path ),
      host ( host ),
      port ( port ),
      saidHello ( false ),
      myStraw ( 0 )
{
    data_dir << "/tmp/qrsh_"
             << getpid();

    if(mkdir ( data_dir.str().c_str(), 0777 ) )
    {
        fprintf ( stderr,
                  "QrshServer::QrshServer error: can't mkdir |%s|\n",
                  data_dir.str().c_str()
                );
        exit ( 1 );
    }
}



void
QrshServer::saveCommand ( Message const & message )
{
    savedCommand = message;
}



void
QrshServer::runSavedCommand ( )
{
    runCommand ( savedCommand );
}



void
QrshServer::start ( )
{
    stringstream announcement_data;
    announcement_data << "hello_my_name_is "
                      << name;

    send ( announcement_data.str() );

    saidHello = true;
}




void
QrshServer::send ( string const & content )
{
    try
    {
        Message message;
        message.setData ( content );

        Connection connection;
        connection.open ( host, port );
        Session session = connection.newSession ( );
        session.messageTransfer ( arg::content     = message,
                                  arg::destination = "amq.fanout"
                                );
        session.close();
        connection.close();
    }
    catch ( exception const & e )
    {
        fprintf ( stderr, "QrshServer::send error: |%s|\n", e.what() );
    }
}




void
QrshServer::sayHello ( )
{
    if ( saidHello )
        return;

    stringstream ss;

    ss << "hello_my_name_is "
       << name;

    send ( ss.str() );
    saidHello = true;
}



void
QrshServer::sayName  ( )
{
    fprintf ( stderr, "My name is: |%s|\n", name.c_str() );
}




void
QrshServer::drawStraws ( )
{
    myStraw = mrand ( 1000000000 );
    stringstream ss;
    ss << "straw "
       << name
       << ' '
       << myStraw;
    send ( ss.str() );
}



void
QrshServer::getStraw ( Message const & message )
{
    int straw;

    char brokerName[1000];
    sscanf ( message.getData().c_str(), "%*s%s", brokerName );

    if ( ! strcmp ( brokerName, name.c_str() ) )
        return;

    sscanf ( message.getData().c_str(), "%*s%*s%d", & straw );
    straws.push_back ( straw );

    bool i_win = true;
    int ties = 0;

    if ( straws.size() >= myFellowBrokers.size() )
    {
        // All votes are in!  Let's see if I win!
        for ( unsigned int i = 0; i < straws.size(); ++ i )
        {
            if ( straws[i] == myStraw )
                ++ ties;
            else
            if ( straws[i] > myStraw )
            {
                i_win = false;
                break;
            }
        }

        if ( i_win && (ties <= 0) )
        {
            myStraw = 0;
            straws.clear();
            runSavedCommand ( );
        }
        else
        if ( i_win && (ties > 0) )
        {
            fprintf ( stderr, "MDEBUG  oh no!  drawStraws error: server %s tied with straw %d!\n", name.c_str(), straw );
        }
    }
}




/*
 * "APB" command (all-points-bullitens (commands that are not addressed
 * specifically to any server)) are handled directly, here.
 * Because if I return simply "true", the normal command processing code
 * will misinterpret the command.
 */
bool
QrshServer::myMessage ( Message const & message )
{
    int const maxlen = 100;
    char head[maxlen];
    char first_word [ maxlen + 1 ];
    strncpy ( head, message.getData().c_str(), maxlen );
    sscanf ( head, "%s", first_word );

    if ( ! strcmp ( name.c_str(), first_word ) )
    {
        return true;
    }
    else
    {
        // Is the given name one of my aliases?
        char possibleAlias[1000];
        if(1 == sscanf ( message.getData().c_str(), "%s", possibleAlias ))
        {
            for ( unsigned int i = 0; i < aliases.size(); ++ i )
            {

              if ( ! strcmp ( possibleAlias, aliases[i]->c_str() ))
              {
                return true;
              }
            }
        }
    }

    if ( ! strcmp ( first_word, "hello_my_name_is" ) )
    {
        rememberIntroduction ( message );
        sayHello ( );
        return false;
    }
    else
    if ( ! strcmp ( first_word, "straw" ) )
    {
        getStraw ( message );
        return false;
    }
    else
    if ( ! strcmp ( first_word, "all" ) )
    {
        return true;
    }
    else
    if ( ! strcmp ( first_word, "any" ) )
    {
        straws.clear();
        usleep ( 200000 );
        saveCommand ( message );
        drawStraws ( );
        return false;
    }
    else
        return false;
}




void
QrshServer::rememberIntroduction ( Message const & message )
{
    char brokerName [ 1000 ];
    sscanf ( message.getData().c_str(), "%*s%s", brokerName );

    if ( strcmp ( brokerName, name.c_str() ) )
        myFellowBrokers.insert ( string ( brokerName ) );
}




void
QrshServer::addAlias ( Message const & message )
{
    char alias[1000];
    sscanf ( message.getData().c_str(), "%*s%*s%s", alias );
    aliases.push_back ( new string(alias) );
}




void
QrshServer::getNames ( )
{
    abstract_name_map.clear();

    DIR * dir = opendir ( data_dir.str().c_str() );

    if ( ! dir )
    {
        fprintf ( stderr,
                  "QrshServer::getNames error: could not open dir |%s|.\n",
                  data_dir.str().c_str()
                );
        return;
    }

    struct dirent * file;
    while ( (file = readdir ( dir ) ) )
    {
        if ( '.' != file->d_name[0] )
        {
            stringstream pid_file_name;
            pid_file_name << data_dir.str()
                          << '/'
                          << file->d_name
                          << "/pid";

            int pid = 0;
            FILE * fp;
            if ( (fp = fopen ( pid_file_name.str().c_str(), "r" ) ) )
            {
                fscanf ( fp, "%d", & pid );
                fclose ( fp );
                abstract_name_map.insert(pair<char*, int>(strdup(file->d_name), pid));
            }
            else
            {
                /*
                 * Fail silently.  The non-existence of this file
                 * is not necessarily an error.
                 */
            }
        }
    }
    closedir ( dir );
}



void
QrshServer::string_replaceAll ( string & str,
                                string & target,
                                string & replacement
                              )
{
    int target_size = target.size();
    int found_pos = 0;

    while ( 0 <= (found_pos = str.find ( target ) ) )
        str.replace ( found_pos, target_size, replacement );
}




bool
QrshServer::isProcessName ( char const * str )
{
    getNames();
    map<char *, int>::iterator it;
    for ( it = abstract_name_map.begin(); it != abstract_name_map.end(); ++ it )
    {
        if ( ! strcmp ( str, it->first ) )
          return true;
    }

    return false;
}





int
QrshServer::string_countWords ( char const * s1 )
{
    int count = 0;
    char const * s2 = s1 + 1;

    if ( ! isspace(* s1) )
    {
        ++ count;
    }

    for ( ; * s2; ++ s1, ++ s2 )
    {
        // count space-to-word transitions.
        if ( isspace(*s1) && (! isspace(*s2)) )
          ++ count;
    }

    return count;
}




void
QrshServer::execute ( Message const & message )
{
    // First, gather all the symbolic names we know.
    getNames();

    // Now make a copy of the command, that I can alter.
    string command ( message.getData() );


    // Replace each occurrence of every abstract name with its pid.
    char pid_str[100];
    map<char *, int>::iterator it;
    for ( it = abstract_name_map.begin(); it != abstract_name_map.end(); ++ it )
    {
        sprintf ( pid_str, "%d", it->second );
        string target      ( it->first ),
               replacement ( pid_str );
        string_replaceAll ( command, target, replacement );
    }


    char const * truncated_command = skipWord(skipWord(command.c_str()));

    if ( truncated_command )
        system ( truncated_command );
}





void
QrshServer::get ( Message const & request_message )
{
    char * file_content;

    /*
     * Get the contents of the requested file.
     */
    char file_or_process_name[1000];
    sscanf ( request_message.getData().c_str(), "%*s%*s%s", file_or_process_name );

    if ( isProcessName ( file_or_process_name ) )
    {
        stringstream desired_file_name;
        desired_file_name << data_dir.str()
                          << '/'
                          << file_or_process_name
                          << '/';
        char requested_output_stream[1000];
        if(1 != sscanf ( request_message.getData().c_str(),
                         "%*s%*s%*s%s",
                         requested_output_stream
                       )
          )
        {
            fprintf ( stderr,
                      "QrshServer::get error: Can't read requested data file name from this message: |%s|\n",
                      request_message.getData().c_str()
                    );
            return;
        }
        desired_file_name << requested_output_stream;
        file_content = file2str ( desired_file_name.str().c_str() );
    }
    else
    {
        file_content = file2str ( file_or_process_name );
    }

    stringstream reply_data ;
    reply_data << "get_response "
               << file_content;
    /*
     * Send a response-message to the server who is waiting.
     */
    send ( reply_data.str() );
}






void
QrshServer::exited ( Message const & message )
{
    int  exit_code = -1;

    // First, gather all the symbolic names we know.
    getNames();

    // Now make a copy of the command, that I can alter.
    string edited_command ( message.getData() );

    // Replace each occurrence of every abstract name with its pid.
    char pid_str[100];
    map<char *, int>::iterator it;
    for ( it = abstract_name_map.begin(); it != abstract_name_map.end(); ++ it )
    {
        sprintf ( pid_str, "%d", it->second );
        string target      ( it->first ),
               replacement ( pid_str );
        string_replaceAll ( edited_command, target, replacement );
    }

    // Skip the service name.  That is not used by the child.
    char const * truncated_command = skipWord(edited_command.c_str());

    if ( truncated_command )
    {
        stringstream ss;
        ss << qrsh_run_path
           << ' '
           << data_dir.str()
           << ' '
           << truncated_command;

        int child_pid;
        if ( ! (child_pid = fork() ) )
        {
            // This is the child.

            char ** argv = getArgs ( ss.str().c_str() );
            execv ( qrsh_run_path.c_str(), argv );

            perror ( "qrsh_server: execv error: " );
            exit ( 1 );
        }
        else
        {
            // This is the parent.
            pid_t awaited_pid;
            while ( 0 == (awaited_pid = waitpid ( child_pid, & exit_code, WNOHANG)) )
            {
                fprintf ( stderr, "qrsh_server info: parent: waiting for child...\n" );
                sleep(1);
            }

            if ( -1 == awaited_pid )
            {
                fprintf ( stderr, "qrsh_server error awaiting child!\n" );
                exit ( 1 );
            }

            exit_code >>= 8;

            stringstream data;
            data << "wait_response "
                 << exit_code;

            send ( data.str() );
        }
    }
}




void
QrshServer::wait ( Message const & message )
{
    bool pre_existing = false;
    if ( 3 == string_countWords ( message.getData().c_str() ) )
    {
        // The first word is the name of this service.
        // The second word is "exec_wait".
        // The third word is the symbolic name of the command to wait for.
        // The fact that there are exactly three words means that this
        // must be a command that has already been named and started --
        // we just need to find its pid and wait on it.
        pre_existing = true;
    }


    int  exit_code = -1;

    // First, gather all the symbolic names we know.
    getNames();

    // Now make a copy of the command, that I can alter.
    string edited_command ( message.getData() );

    // Replace each occurrence of every abstract name with its pid.
    char pid_str[100];
    map<char *, int>::iterator it;
    for ( it = abstract_name_map.begin(); it != abstract_name_map.end(); ++ it )
    {
        sprintf ( pid_str, "%d", it->second );
        string target      ( it->first ),
               replacement ( pid_str );
        string_replaceAll ( edited_command, target, replacement );
    }

    // Skip the service name.  That is not used by the child.
    char const * truncated_command = skipWord(edited_command.c_str());

    if ( truncated_command )
    {
        stringstream ss;
        ss << qrsh_run_path
           << ' '
           << data_dir.str()
           << ' '
           << truncated_command;

        int child_pid;
        if ( ! (child_pid = fork() ) )
        {
            // This is the child.

            char ** argv = getArgs ( ss.str().c_str() );
            execv ( qrsh_run_path.c_str(), argv );

            perror ( "qrsh_server: execv error: " );
            exit ( 1 );
        }
        else
        {
            // This is the parent.
            pid_t awaited_pid;
            while ( 0 == (awaited_pid = waitpid ( child_pid, & exit_code, WNOHANG)) )
            {
                fprintf ( stderr, "qrsh_server info: parent: waiting for child...\n" );
                sleep(1);
            }

            if ( -1 == awaited_pid )
            {
                fprintf ( stderr, "qrsh_server error awaiting child!\n" );
                exit ( 1 );
            }
        }

        exit_code >>= 8;

        stringstream data;
        data << "wait_response "
             << exit_code;

        send ( data.str() );
    }
}





char const *
QrshServer::skipWord ( char const * s )
{
    if(! (s && *s) )
        return 0;

    // skip past initial white space
    while ( isspace(*s) )
    {
        ++ s;
        if(! *s)
            return 0;
    }

    // skip past first word
    while ( ! isspace(*s) )
    {
        ++ s;
        if(! *s)
            return 0;
    }

    return s;
}





char **
QrshServer::getArgs ( char const * str )
{
    char const * s = str;

    char ** argv = 0;
    vector<int> start_positions,
                lengths;

    int pos     = 0;
    int arg_len = 0;

    int n_args = 0;
    while ( 1 )
    {
        // advance over whitespace.
        while ( isspace ( *s ) )
        {
            ++ s; ++ pos;
            if(! *s)
            {
                goto done;
            }
        }

        ++ n_args;
        start_positions.push_back ( pos );
        arg_len = 0;

        // advance over non-whitespace.
        while ( ! isspace ( *s ) )
        {
            ++ s; ++ pos; ++ arg_len;
            if(! *s)
            {
                lengths.push_back ( arg_len );
                arg_len = 0;
                goto done;
            }
        }

        lengths.push_back ( arg_len );
        arg_len = 0;
    }

    done:

    if ( arg_len > 0 )
        lengths.push_back ( arg_len );

    // Alloc the array.
    argv = (char **) malloc ( sizeof(char *) * ( n_args + 1 ) );
    argv[n_args] = 0; // mull-term the array.

    for ( int i = 0; i < n_args; ++ i )
    {
        argv[i] = ( char *) malloc ( lengths[i] + 1 );
        strncpy ( argv[i],
                  str + start_positions[i],
                  lengths[i]
                );
        argv[i][lengths[i]] = 0;
    }

    return argv;
}



void
QrshServer::runCommand ( Message const & message )
{
    char const * s = message.getData().c_str();

    /*
     * Skip the first word, which is this server's name.
     */
    while ( isspace(*s) )     // go to start of first word.
        ++ s;

    while ( ! isspace(*s) )   // go to end of first word.
        ++ s;

    while ( isspace(*s) )     // go to start of second word.
        ++ s;

    char command_name[1000];
    sscanf ( s, "%s", command_name );

    if ( ! strcmp ( "get", command_name ) )
    {
        get ( message );
    }
    else
    if ( ! strcmp ( "exited", command_name ) )
    {
        exited ( message );
    }
    else
    if ( ! strcmp ( "exec_wait", command_name ) )
    {
        wait ( message );
    }
    else
    if ( ! strcmp ( "exec", command_name ) )
    {
        execute ( message );
    }
    else
    if ( ! strcmp ( "start", command_name ) )
    {
        start ( );
    }
    else
    if ( ! strcmp ( "alias", command_name ) )
    {
      addAlias ( message );
    }
    else
    if ( ! strcmp ( "sayName", command_name ) )
    {
      sayName ( );
    }
    else
    {
        /*
         * If the command is not any of the "special" commands
         * above, then it's a "normal" command.
         * That means we run it with a child process instance of
         * qrsh_run, which will save all its data in the qrsh dir.
         */
        stringstream ss;
        ss << qrsh_run_path
           << ' '
           << data_dir.str()
           << ' '
           << s;

        if ( ! fork() )
        {
            char ** argv = getArgs ( ss.str().c_str() );
            execv ( qrsh_run_path.c_str(), argv );
            perror ( "qrsh_server: execv error: " );
        }
    }
}



void
QrshServer::received ( Message & message )
{
    if ( myMessage ( message ) )
        runCommand ( message );
}



}} // namespace qpid::tests

using namespace qpid::tests;

/*
 *  fixme mick Mon Aug  3 10:29:26 EDT 2009
 *  argv[1] server name
 *  argv[2] qrsh exe path
 *  argv[3] host
 *  argv[4] port
 */
int
main ( int /*argc*/, char** argv )
{
    const char* host = argv[3];
    int port = atoi(argv[4]);
    Connection connection;
    Message msg;

    srand ( getpid() );

    try
    {
        connection.open ( host, port );
        Session session =  connection.newSession();


        // Declare queues.
        string myQueue = session.getId().getName();
        session.queueDeclare ( arg::queue=myQueue,
                               arg::exclusive=true,
                               arg::autoDelete=true);

        session.exchangeBind ( arg::exchange="amq.fanout",
                               arg::queue=myQueue,
                               arg::bindingKey="my-key");

        // Create a server and subscribe it to my queue.
        SubscriptionManager subscriptions ( session );
        QrshServer server ( subscriptions,
                            argv[1],         // server name
                            argv[2],         // qrsh exe path
                            host,
                            port
                          );
        subscriptions.subscribe ( server, myQueue );

        // Receive messages until the subscription is cancelled
        // by QrshServer::received()
        subscriptions.run();

        connection.close();
    }
    catch(const exception& error)
    {
        cout << error.what() << endl;
        return 1;
    }

    return 0;
}




