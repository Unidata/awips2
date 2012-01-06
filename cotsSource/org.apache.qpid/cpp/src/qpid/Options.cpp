/*
 *
 * Copyright (c) 2006 The Apache Software Foundation
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */

#include "qpid/Options.h"
#include "qpid/Exception.h"

#include <boost/bind.hpp>

#include <fstream>
#include <algorithm>
#include <iostream>

namespace qpid {

using namespace std;


/*
 *  ---------------------------------------------
 *  Explanation for Boost 103200 conditional code
 *  ---------------------------------------------
 *
 *  Please see large comment in Options.h .
 *
 */

#if ( BOOST_VERSION == 103200 )
std::vector<std::string> Options::long_names;
std::vector<std::string> Options::short_names;
#endif




namespace {

struct EnvOptMapper {
    static bool matchChar(char env, char opt) {
        return (env==toupper(opt)) || (strchr("-.", opt) && env=='_');
    }

    static bool matchStr(const string& env, boost::shared_ptr<po::option_description> desc) {
        return desc->long_name().size() == env.size() &&
            std::equal(env.begin(), env.end(), desc->long_name().begin(), &matchChar);
    }
            
    static bool matchCase(const string& env, boost::shared_ptr<po::option_description> desc) {
        return env == desc->long_name();
    }
            
    EnvOptMapper(const Options& o) : opts(o) {}
    
    string operator()(const string& envVar) {
        static const std::string prefix("QPID_");
        if (envVar.substr(0, prefix.size()) == prefix) {
            string env = envVar.substr(prefix.size());
#if (BOOST_VERSION >= 103300)
            typedef const std::vector< boost::shared_ptr<po::option_description> > OptDescs;
            OptDescs::const_iterator i =
                find_if(opts.options().begin(), opts.options().end(), boost::bind(matchStr, env, _1));
            if (i != opts.options().end())
                return (*i)->long_name();
#else
            /*
             *  For Boost version 103200 and below.
             *
             *  In Boost version 103200, the options_description::options member,
             *  used above, is private.  So what I will do here is use the 
             *  count() funtion, which returns a 1 or 0 indicating presence or
             *  absence of the environment variable.  
             * 
             * If it is present, I will return its name.  Env vars do not have
             *  short and long forms, so the name is synonymous with the long 
             *  name.  (This would not work for command line args.)
             *  And if it's absent -- an empty string.
             */


            /*
             * The env vars come in unaltered, i.e. QPID_FOO, but the 
             * options are stored normalized as "qpid-foo".  Change the
             * local variable "env" so it can be found by "opts".
             */ 
            for (std::string::iterator i = env.begin(); i != env.end(); ++i) 
            {
                *i = (*i == '_') 
                     ? '-' 
                     : ::tolower(*i);
            }

            if ( opts.count(env.c_str()) > 0 )
            {
              return env.c_str();
            }
            else
            {
              return string();
            }
#endif
        }
        return string();
    }


    bool
    isComment ( string const & str )
    {
      size_t i = str.find_first_not_of ( " \t" );

      if ( i == string::npos )
        return true;

      return str[i] == '#';
    }


    string configFileLine (string& line) {
        
        if ( isComment ( line ) )
          return string();

        size_t pos = line.find ('=');
        if (pos == string::npos)
            return string();
        string key = line.substr (0, pos);
#if (BOOST_VERSION >= 103300)
        typedef const std::vector< boost::shared_ptr<po::option_description> > OptDescs;
        OptDescs::const_iterator i = 
            find_if(opts.options().begin(), opts.options().end(), boost::bind(matchCase, key, _1));
        if (i != opts.options().end())
            return string (line) + "\n";
        else
          return string();
#else
        // Use 'count' to see if this option exists.  Using 'find' will SEGV or hang
        // if the option has not been defined yet.
        if ( opts.count(key.c_str()) > 0 )
          return string ( line ) + "\n";
        else
          return string ( );
#endif
    }

    const Options& opts;
};

}
std::string prettyArg(const std::string& name, const std::string& value) {
    return value.empty() ? name+" " : name+" ("+value+") ";
}

Options::Options(const string& name) : 
  po::options_description(name) 

#if ( BOOST_VERSION == 103200 )
  , m_less_easy(this, this)
#endif
{
}





void Options::parse(int argc, char const* const* argv, const std::string& configFile, bool allowUnknown)
{
    string defaultConfigFile = configFile; // May be changed by env/cmdline
    string parsing;
    try {
        po::variables_map vm;
        parsing="command line options";
        if (argc > 0 && argv != 0) {
            if (allowUnknown) {
#if (BOOST_VERSION >= 103300)
                // This hideous workaround is required because boost 1.33 has a bug
                // that causes 'allow_unregistered' to not work.
                po::command_line_parser clp = po::command_line_parser(argc, const_cast<char**>(argv)).
                    options(*this).allow_unregistered();
                po::parsed_options opts     = clp.run();
                po::parsed_options filtopts = clp.run();
                filtopts.options.clear ();
                for (std::vector< po::basic_option<char> >::iterator i = opts.options.begin();
                     i != opts.options.end(); i++)
                    if (!i->unregistered)
                        filtopts.options.push_back (*i);
                po::store(filtopts, vm);

#elif ( BOOST_VERSION == 103200 )

      /*
       * "Tokenize" the argv to get rid of equals signs.
       */
      vector<string> tokenized_argv;
      vector<string>::iterator iter;

      for ( int i = 0; i < argc; ++ i )
      {
        string s = argv[i];
        size_t equals_pos = s.find_first_of ( '=' );

        if ( string::npos == equals_pos )  // There's no equals sign.  This is a token.
        {
          tokenized_argv.push_back(s);
        }
        else
        {
          // Two tokens -- before and after the equals position.
          tokenized_argv.push_back ( s.substr(0, equals_pos) );
          tokenized_argv.push_back ( s.substr(equals_pos+1) );
        }
      }


      /*
       * Now "filter" the tokenized argv, to get rid of all
       * unrecognized options.  Because Boost 103200 has no
       * facility for dealing gracefully with "unregistered" 
       * options.
       */
      vector<string>            filtered_argv;
      vector<string>::iterator  the_end = tokenized_argv.end();

      // The program-name gets in for free...
      iter = tokenized_argv.begin();
      filtered_argv.push_back ( * iter );
      ++ iter;

      // ...but all other args get checked.
      while ( iter < the_end )
      {
       /*
        * If this is an argument that is registered,
        * copy it to filtered_argv and also copy all
        * of its arguments.
        */
       if ( is_registered_option ( * iter ) )
       {
         // Store this recognized arg.
         filtered_argv.push_back ( * iter );
         ++ iter;

         // Copy all values for the above arg.
         // Args are tokens that do not start with a minus.
         while ( (iter < the_end) && ((* iter)[0] != '-') )
         {
           filtered_argv.push_back ( * iter );
           ++ iter;
         }
       }
       else
       {
         // Skip this unrecognized arg.
         ++ iter;

         // Copy all values for the above arg.
         // Values are tokens that do not start with a minus.
         while ( (iter < the_end) && ( '-' != (*iter)[0] ) )
         {
           ++ iter;
         }
       }
     }

     // Make an array of temporary C strings, because 
     // the interface I can use wants it that way.
     int     new_argc = filtered_argv.size();
     char ** new_argv = new char * [ new_argc ];
     int i = 0;

     // cout << "NEW ARGV: |";
     for ( iter = filtered_argv.begin();
           iter < filtered_argv.end();
           ++ iter, ++ i
         )
     {
       new_argv[i] = strdup( (* iter).c_str() );
       // cout << " " << new_argv[i] ;
     }
     // cout << "|\n";


     // Use the array of C strings.
     po::basic_parsed_options<char> bpo = po::parse_command_line(new_argc, const_cast<char**>(new_argv), *this);
     po::store(bpo, vm);


     // Now free the temporary C strings.
     for ( i = 0; i < new_argc; ++ i )
     {
       free ( new_argv[i] );
     }
     delete[] new_argv;

#endif
            }
            else
                po::store(po::parse_command_line(argc, const_cast<char**>(argv), *this), vm);
        }
        parsing="environment variables";
        po::store(po::parse_environment(*this, EnvOptMapper(*this)), vm);
        po::notify(vm); // configFile may be updated from arg/env options.
        if (!configFile.empty()) {
            parsing="configuration file "+configFile;
            ifstream conf(configFile.c_str());
            if (conf.good()) {
                // Remove this hack when we get a stable version of boost that
                // can allow unregistered options in config files.
                EnvOptMapper mapper(*this);
                stringstream filtered;

                while (!conf.eof()) {
                    string line;
                    getline (conf, line);
                    filtered << mapper.configFileLine (line);
                }

                po::store(po::parse_config_file(filtered, *this), vm);
                // End of hack
            }
            else {
                // No error if default configfile is missing/unreadable
                // but complain for non-default config file.
                if (configFile != defaultConfigFile)
                    throw Exception("cannot read configuration file "
                                    +configFile);
            }
        }
        po::notify(vm);
    }
    catch (const std::exception& e) {
        ostringstream msg;
        msg << "Error in " << parsing << ": " << e.what() << endl;
#if (BOOST_VERSION >= 103300)
        if (find_nothrow("help", false))
            msg << "Use --help to see valid options" << endl;
#endif
        throw Exception(msg.str());
    }
}

CommonOptions::CommonOptions(const string& name, const string& configfile)
    : Options(name), config(configfile)
{
    addOptions()
        ("help,h", optValue(help), "Displays the help message")
        ("version,v", optValue(version), "Displays version information")
        ("config", optValue(config, "FILE"), "Reads configuration from FILE");
}




#if ( BOOST_VERSION == 103200 )
options_description_less_easy_init&
options_description_less_easy_init::operator()(char const * name,
           char const * description)
{
  // Snoop on the arguments....
  owner->register_names ( name );
  // ... then call parent function explicitly.
  po::options_description_easy_init::operator() ( name, description );
  return * this;
}


options_description_less_easy_init&
options_description_less_easy_init::operator()(char const * name,
           const po::value_semantic* s)
{
  // Snoop on the arguments....
  owner->register_names ( name );
  // ... then call parent function explicitly.
  po::options_description_easy_init::operator() ( name, s );
  return * this;
}


options_description_less_easy_init&
options_description_less_easy_init::operator()(const char* name,
           const po::value_semantic* s,
           const char* description)
{
  // Snoop on the arguments....
  owner->register_names ( name );
  // ... then call parent function explicitly.
  po::options_description_easy_init::operator() ( name, s, description );
  return * this;
}





void
Options::register_names ( std::string s )
{
  
  std::string::size_type comma_pos = s.find_first_of ( ',' );

  if ( std::string::npos == comma_pos )
  {
    // There is no short-name.
    long_names.push_back ( s );
  }
  else
  {
    std::string long_name  = s.substr(0, comma_pos),
                short_name = s.substr(comma_pos+1);
    long_names .push_back ( long_name );
    short_names.push_back ( short_name );
  }
  
  /*
   * There is no way to tell when the adding of new options is finished,
   * so I re-sort after each one.
   */
  std::sort ( long_names .begin(), long_names .end() );
  std::sort ( short_names.begin(), short_names.end() );
}





bool 
Options::is_registered_option ( std::string s )
{
  std::string without_dashes = s.substr ( s.find_first_not_of ( '-' ) );
  std::vector<std::string>::iterator i;

  // Look among the long names.
  i = std::find ( long_names.begin(),
                  long_names.end(),
                  without_dashes
                );
  if ( i != long_names.end() )
    return true;

  // Look among the short names.
  i = std::find ( short_names.begin(),
                  short_names.end(),
                  without_dashes
                );
  if ( i != short_names.end() )
    return true;


  return false;
}
#endif


} // namespace qpid

