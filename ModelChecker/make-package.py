import sys
from subprocess import call
import os
import shutil

"""Prints the syntax and options to the user"""
def print_usage():
    print("\nThis command packs the relevant binary files in a jar. The output jar is stored in the release folder.")
    print("\nSyntax : make-package release-name [options]","\n\nrelease-name can be any of the following :")
    print("\t. -ctl-only : packs ctl-related classes only\n\t. -ast-only : packs ast-related classes only\n\t. -full : packs the whole application")
    print("\n-----------------\nOptions\n-----------------\n\n. -src : adds the corresponding source code to the archive")

"""Packs together the packages passed as a parameter"""
def make_jar(folder,build,packages):
    root = os.getcwd()
    os.chdir(folder)
    print("current",os.getcwd())
    call([ "jar","-cf","../release/tmp/" + build[1:] + "-" + folder + ".jar" ] + packages)
    os.chdir(root)

def main():
    if len(sys.argv) < 2:
        print_usage()
        return

    build = sys.argv[1] 
    if build == "-ctl-only":
        packages = [ "ctl","graph" ]
    elif build == "-ast-only":
        packages = [ "ast","graph" ]
    elif build == "-full":
        packages = [ "ast","ctl","cfg","graph" ]
    else: 
        print_usage()
        return
    
    # create a temporary folder to contain the sub-jars
    if not(os.path.exists("release/tmp")):
        os.mkdir("release/tmp")
    make_jar("bin",build,packages)

    if "-src" in sys.argv:
        make_jar("src",build,packages)
    
    # package the sub-jars together
    os.chdir("release")
    call([ "jar","-cf",build[1:] + ".jar","-C","tmp","." ])
    shutil.rmtree("tmp")

if __name__ == "__main__":
    main()
