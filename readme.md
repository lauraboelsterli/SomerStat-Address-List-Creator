# SomerStat-Template

This is a template repo for SomerStat

## Description

Template repositories are examples that can be copied when new repositories are added. This can help simplify setting up a repository and remembering / executing the steps it takes to make things good. We can have a few templates, one for R projects, etc.

With a new project, delete anything that's not helpful!

### Parts

#### Data, Scripts, and Templates Folders

Projects should have folders for data and scripts, as well as figures if applicable. Within data, there should be in and out directories to the extent that it's useful. It's generally recommended not to edit inputs to your code so that you can re-run without re-sourcing input data, so it makes sense to keep separate from data you create and will want to go look at.

##### .gitkeep

Git can't save empty directories, so the way to have them in a way that makes sense to Git is to have a file within them. A blank file of any kind is theoretically fine, but empty .gitkeep files is a useful convention.

#### .gitignore

The default gitignore will exclude specific R files that are unhelpful in general and particularly illegible for humans. Of course add to this file to add files that should be ignore *for all users*. You can update your .git/info/exclude file with things you yourself want to ignore, like temp files, etc.

#### readme.md

The readme.md is where code-related documentation goes. This is independent from other project management documentation that is broadly usable without code, which should exist outside of repositories. 

You can also add readme.md files to any folder and GitHub will always render them, and they can be linked in the main readme.md file as well. This can be a little up to preference!

#### GitHub.url

This is a web shortcut to the repository in GitHub. It should always have this name and can be edited by dragging it from a Windows Explorer window to a text editor. Right clicking in Windows Explorer doesn't seem to help much.

The reasoning behind this being a part of the template and tracked here is that it'll be the same for everybody and will only have to be done one time per repository.

#### Project Folder.txt

This file is similar to GitHub.url in that it's kept here for the purposes of fewer people needing to go through the pain of setting it up and so that it's the same for everybody. This file is a simple one-line text file that has a user-agnostic path to the Project Folder in OneDrive where non-code related files live. If it changes, this file should be changed to reflect!

Changes here can be made with any text editor and can use the template version as a guide!

#### Project Folder.lnk

This file is *not* tracked by Git, but it's included in .gitignore as a thing for Git to not pay attention to across all users. The idea with this file is that everybody will create a Windows Shortcut on their own computer to live in their local Git repo for easy navigating to the place where non-Git files live. 

To create this file, right click in the local repo and create a shortcut. Paste the exact text from Project Folder.txt into it, and name it Project Folder. The naming only matters here so that it's caught by the .gitignore statement. If you want to name it something else and ignore it via .git/info/exclude, go nuts!

## Dependencies

### Installing

Installing a markdown editor may be required.

#### RStudio

RStudio as lame as it is as a Git interface is a pretty fantastic markdown editor. What makes me say this is the "Visual" editing mode when editing a .md file:

![a0587ffa52fb588c0c47f444814361074007db77.png](md-images/24a07417f2a04f7b5a0dddf6efc39a63a085d5c8.png)

This renders the markdown language as you edit, allowing you to choose styles and lists as you would in something like Word (WordPad, really), rather than GitHub's editor resembling something closer to notepad, where you need to remember / reference the different ways to indicate styles, lists, etc.

#### Marktext

Another markdown editor that's fantastic is called marktext. It's downloadable (and lets you specify user-only install!!!!) [here](https://github.com/marktext/marktext/releases). Grab the marktext-setup.exe file from whichever version you like (top one seems like it's always supposed to be latest stable). It's *slightly* more quirky to start with (menus aren't visible, etc.) but the right things pop up on the screen when you need them in an unusually deliberate and omniscient way. It's fun!

#### Executing program

To create a repo from this, you select it in the normal menu when creating a repository. All staff are certainly authorized to do this (which requries logging into @SomerStat GitHub account), but that's not the general way of creating a repo, so there should be another way.

This other way is to use the batch file in SomerStat - Documents\Tools\Git Repo Creator\ called create-new-repo-from-template.bat. This will create the repo from template and open the repo in a browser to be renamed and described and such. Super easy and fast!

## Authors

Sam Shaffer

## Future Development

- [ ] Create different templates for different types of projects

- [ ] Create multiple batch files to create different types of projects (different team assignments, etc.)

- [ ] Build all batches into same interactive batch with series of options and menu

## Major Update History

## Appendix

### File Directory

- Example Directory
  
  - [Example Directory/Example
    File.txt](#example-directoryexample-filetxt)
    - Note that section selectors have:
      - All lowercase letters
      - No slashes
      - No periods
      - One # character
      - No space after #
      - Maybe other things, please edit if necessary!

#### Example Directory/Example File.txt

Example Description 
