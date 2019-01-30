require 'mkmf'

if RbConfig::CONFIG['CC'] =~ /gcc/
  $CFLAGS << " -O3" unless $CFLAGS[/-O\d/]
  $CFLAGS << " -Wall -Wcast-qual -Wwrite-strings -Wmissing-noreturn -Winline"
end

create_makefile('llrbtree/llrbtree')
