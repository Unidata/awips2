= INSTALLATION =

Extract the release archive into a directory of your choice and set
your RUBYLIB environment variable accordingly:

  tar -xzf qpid-ruby-<version>.tar.gz -C <install-prefix>
  export RUBYLIB=<install-prefix>/qpid-<version>/ruby/lib

= GETTING STARTED =

The ruby client includes a simple hello-world example that publishes
and consumes a message:

  cp <install-prefix>/qpid-<version>/ruby/examples/hello-world.rb .
  ./hello-world.rb

= RUNNING THE TESTS =

The "tests" directory contains a collection of unit tests for the ruby
client. These can be run with the Rakefile provided:

  rake test
