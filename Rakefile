require 'rake'
require 'fileutils'
require 'rake/clean'


task :default => :doc

task :doc do
  command = "npx docco ./test/*.js ./lib/*.js -c docs/css/docco.css -t docs/template/docco.jst"
  # command = "docker run -it --rm -v $PWD:/usr/src/app registry.homeunix.net:5000/emile/nodejs-labo-6.10:0.7"
  sh command
end
