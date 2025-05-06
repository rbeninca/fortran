# Microsoft Developer Studio Generated NMAKE File, Format Version 4.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Console Application" 0x0103

!IF "$(CFG)" == ""
CFG=programa8_3 - Win32 Debug
!MESSAGE No configuration specified.  Defaulting to programa8_3 - Win32 Debug.
!ENDIF 

!IF "$(CFG)" != "marchi - Win32 Release" && "$(CFG)" != "marchi - Win32 Debug"\
 && "$(CFG)" != "programa12_1 - Win32 Release" && "$(CFG)" !=\
 "programa12_1 - Win32 Debug" && "$(CFG)" != "programa7_1 - Win32 Release" &&\
 "$(CFG)" != "programa7_1 - Win32 Debug" && "$(CFG)" !=\
 "programa7_1a - Win32 Release" && "$(CFG)" != "programa7_1a - Win32 Debug" &&\
 "$(CFG)" != "programa7_1b - Win32 Release" && "$(CFG)" !=\
 "programa7_1b - Win32 Debug" && "$(CFG)" != "programa8_1b - Win32 Release" &&\
 "$(CFG)" != "programa8_1b - Win32 Debug" && "$(CFG)" !=\
 "programa8_6 - Win32 Release" && "$(CFG)" != "programa8_6 - Win32 Debug" &&\
 "$(CFG)" != "programa8_2 - Win32 Release" && "$(CFG)" !=\
 "programa8_2 - Win32 Debug" && "$(CFG)" != "programa8_3 - Win32 Release" &&\
 "$(CFG)" != "programa8_3 - Win32 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE on this makefile
!MESSAGE by defining the macro CFG on the command line.  For example:
!MESSAGE 
!MESSAGE NMAKE /f "marchi.mak" CFG="programa8_3 - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "marchi - Win32 Release" (based on "Win32 (x86) Console Application")
!MESSAGE "marchi - Win32 Debug" (based on "Win32 (x86) Console Application")
!MESSAGE "programa12_1 - Win32 Release" (based on\
 "Win32 (x86) Console Application")
!MESSAGE "programa12_1 - Win32 Debug" (based on\
 "Win32 (x86) Console Application")
!MESSAGE "programa7_1 - Win32 Release" (based on\
 "Win32 (x86) Console Application")
!MESSAGE "programa7_1 - Win32 Debug" (based on\
 "Win32 (x86) Console Application")
!MESSAGE "programa7_1a - Win32 Release" (based on\
 "Win32 (x86) Console Application")
!MESSAGE "programa7_1a - Win32 Debug" (based on\
 "Win32 (x86) Console Application")
!MESSAGE "programa7_1b - Win32 Release" (based on\
 "Win32 (x86) Console Application")
!MESSAGE "programa7_1b - Win32 Debug" (based on\
 "Win32 (x86) Console Application")
!MESSAGE "programa8_1b - Win32 Release" (based on\
 "Win32 (x86) Console Application")
!MESSAGE "programa8_1b - Win32 Debug" (based on\
 "Win32 (x86) Console Application")
!MESSAGE "programa8_6 - Win32 Release" (based on\
 "Win32 (x86) Console Application")
!MESSAGE "programa8_6 - Win32 Debug" (based on\
 "Win32 (x86) Console Application")
!MESSAGE "programa8_2 - Win32 Release" (based on\
 "Win32 (x86) Console Application")
!MESSAGE "programa8_2 - Win32 Debug" (based on\
 "Win32 (x86) Console Application")
!MESSAGE "programa8_3 - Win32 Release" (based on\
 "Win32 (x86) Console Application")
!MESSAGE "programa8_3 - Win32 Debug" (based on\
 "Win32 (x86) Console Application")
!MESSAGE 
!ERROR An invalid configuration is specified.
!ENDIF 

!IF "$(OS)" == "Windows_NT"
NULL=
!ELSE 
NULL=nul
!ENDIF 
################################################################################
# Begin Project
# PROP Target_Last_Scanned "programa8_1b - Win32 Debug"
F90=fl32.exe
RSC=rc.exe

!IF  "$(CFG)" == "marchi - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Release"
# PROP Intermediate_Dir "Release"
# PROP Target_Dir ""
OUTDIR=.\Release
INTDIR=.\Release

ALL : 

CLEAN : 
	-@erase 

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

# ADD BASE F90 /Ox /I "Release/" /c /nologo
# ADD F90 /Ox /I "Release/" /c /nologo
F90_PROJ=/Ox /I "Release/" /c /nologo /Fo"Release/" 
F90_OBJS=.\Release/
# ADD BASE RSC /l 0x416 /d "NDEBUG"
# ADD RSC /l 0x416 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/marchi.bsc" 
BSC32_SBRS=
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib /nologo /subsystem:console /machine:I386
# ADD LINK32 kernel32.lib /nologo /subsystem:console /machine:I386
LINK32_FLAGS=kernel32.lib /nologo /subsystem:console /incremental:no\
 /pdb:"$(OUTDIR)/marchi.pdb" /machine:I386 /out:"$(OUTDIR)/marchi.exe" 
LINK32_OBJS=

!ELSEIF  "$(CFG)" == "marchi - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "Debug"
# PROP Target_Dir ""
OUTDIR=.\Debug
INTDIR=.\Debug

ALL : 

CLEAN : 
	-@erase 

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

# ADD BASE F90 /Zi /I "Debug/" /c /nologo
# ADD F90 /Zi /I "Debug/" /c /nologo
F90_PROJ=/Zi /I "Debug/" /c /nologo /Fo"Debug/" /Fd"Debug/marchi.pdb" 
F90_OBJS=.\Debug/
# ADD BASE RSC /l 0x416 /d "_DEBUG"
# ADD RSC /l 0x416 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/marchi.bsc" 
BSC32_SBRS=
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib /nologo /subsystem:console /debug /machine:I386
# ADD LINK32 kernel32.lib /nologo /subsystem:console /debug /machine:I386
LINK32_FLAGS=kernel32.lib /nologo /subsystem:console /incremental:yes\
 /pdb:"$(OUTDIR)/marchi.pdb" /debug /machine:I386 /out:"$(OUTDIR)/marchi.exe" 
LINK32_OBJS=

!ELSEIF  "$(CFG)" == "programa12_1 - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "programa12_1\Release"
# PROP BASE Intermediate_Dir "programa12_1\Release"
# PROP BASE Target_Dir "programa12_1"
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "programa12_1\Release"
# PROP Intermediate_Dir "programa12_1\Release"
# PROP Target_Dir "programa12_1"
OUTDIR=.\programa12_1\Release
INTDIR=.\programa12_1\Release

ALL : "$(OUTDIR)\programa12_1.exe"

CLEAN : 
	-@erase ".\programa12_1\Release\programa12_1.exe"
	-@erase ".\programa12_1\Release\programa12_1.obj"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

# ADD BASE F90 /Ox /I "programa12_1\Release/" /c /nologo
# ADD F90 /Ox /I "programa12_1\Release/" /c /nologo
F90_PROJ=/Ox /I "programa12_1\Release/" /c /nologo /Fo"programa12_1\Release/" 
F90_OBJS=.\programa12_1\Release/
# ADD BASE RSC /l 0x416 /d "NDEBUG"
# ADD RSC /l 0x416 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/programa12_1.bsc" 
BSC32_SBRS=
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib /nologo /subsystem:console /machine:I386
# ADD LINK32 kernel32.lib /nologo /subsystem:console /machine:I386
LINK32_FLAGS=kernel32.lib /nologo /subsystem:console /incremental:no\
 /pdb:"$(OUTDIR)/programa12_1.pdb" /machine:I386\
 /out:"$(OUTDIR)/programa12_1.exe" 
LINK32_OBJS= \
	".\programa12_1\Release\programa12_1.obj"

"$(OUTDIR)\programa12_1.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "programa12_1 - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "programa12_1\Debug"
# PROP BASE Intermediate_Dir "programa12_1\Debug"
# PROP BASE Target_Dir "programa12_1"
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "programa12_1\Debug"
# PROP Intermediate_Dir "programa12_1\Debug"
# PROP Target_Dir "programa12_1"
OUTDIR=.\programa12_1\Debug
INTDIR=.\programa12_1\Debug

ALL : "$(OUTDIR)\programa12_1.exe"

CLEAN : 
	-@erase ".\programa12_1\Debug\programa12_1.exe"
	-@erase ".\programa12_1\Debug\programa12_1.obj"
	-@erase ".\programa12_1\Debug\programa12_1.ilk"
	-@erase ".\programa12_1\Debug\programa12_1.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

# ADD BASE F90 /Zi /I "programa12_1\Debug/" /c /nologo
# ADD F90 /Zi /I "programa12_1\Debug/" /c /nologo
F90_PROJ=/Zi /I "programa12_1\Debug/" /c /nologo /Fo"programa12_1\Debug/"\
 /Fd"programa12_1\Debug/marchi.pdb" 
F90_OBJS=.\programa12_1\Debug/
# ADD BASE RSC /l 0x416 /d "_DEBUG"
# ADD RSC /l 0x416 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/programa12_1.bsc" 
BSC32_SBRS=
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib /nologo /subsystem:console /debug /machine:I386
# ADD LINK32 kernel32.lib /nologo /subsystem:console /debug /machine:I386
LINK32_FLAGS=kernel32.lib /nologo /subsystem:console /incremental:yes\
 /pdb:"$(OUTDIR)/programa12_1.pdb" /debug /machine:I386\
 /out:"$(OUTDIR)/programa12_1.exe" 
LINK32_OBJS= \
	".\programa12_1\Debug\programa12_1.obj"

"$(OUTDIR)\programa12_1.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "programa7_1 - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "programa7_1\Release"
# PROP BASE Intermediate_Dir "programa7_1\Release"
# PROP BASE Target_Dir "programa7_1"
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "programa7_1\Release"
# PROP Intermediate_Dir "programa7_1\Release"
# PROP Target_Dir "programa7_1"
OUTDIR=.\programa7_1\Release
INTDIR=.\programa7_1\Release

ALL : "$(OUTDIR)\programa7_1.exe"

CLEAN : 
	-@erase ".\programa7_1\Release\programa7_1.exe"
	-@erase ".\programa7_1\Release\programa7_1.obj"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

# ADD BASE F90 /Ox /I "programa7_1\Release/" /c /nologo
# ADD F90 /Ox /I "programa7_1\Release/" /c /nologo
F90_PROJ=/Ox /I "programa7_1\Release/" /c /nologo /Fo"programa7_1\Release/" 
F90_OBJS=.\programa7_1\Release/
# ADD BASE RSC /l 0x416 /d "NDEBUG"
# ADD RSC /l 0x416 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/programa7_1.bsc" 
BSC32_SBRS=
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib /nologo /subsystem:console /machine:I386
# ADD LINK32 kernel32.lib /nologo /subsystem:console /machine:I386
LINK32_FLAGS=kernel32.lib /nologo /subsystem:console /incremental:no\
 /pdb:"$(OUTDIR)/programa7_1.pdb" /machine:I386 /out:"$(OUTDIR)/programa7_1.exe"\
 
LINK32_OBJS= \
	".\programa7_1\Release\programa7_1.obj"

"$(OUTDIR)\programa7_1.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "programa7_1 - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "programa7_1\Debug"
# PROP BASE Intermediate_Dir "programa7_1\Debug"
# PROP BASE Target_Dir "programa7_1"
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "programa7_1\Debug"
# PROP Intermediate_Dir "programa7_1\Debug"
# PROP Target_Dir "programa7_1"
OUTDIR=.\programa7_1\Debug
INTDIR=.\programa7_1\Debug

ALL : "$(OUTDIR)\programa7_1.exe"

CLEAN : 
	-@erase ".\programa7_1\Debug\programa7_1.exe"
	-@erase ".\programa7_1\Debug\programa7_1.obj"
	-@erase ".\programa7_1\Debug\programa7_1.ilk"
	-@erase ".\programa7_1\Debug\programa7_1.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

# ADD BASE F90 /Zi /I "programa7_1\Debug/" /c /nologo
# ADD F90 /Zi /I "programa7_1\Debug/" /c /nologo
F90_PROJ=/Zi /I "programa7_1\Debug/" /c /nologo /Fo"programa7_1\Debug/"\
 /Fd"programa7_1\Debug/marchi.pdb" 
F90_OBJS=.\programa7_1\Debug/
# ADD BASE RSC /l 0x416 /d "_DEBUG"
# ADD RSC /l 0x416 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/programa7_1.bsc" 
BSC32_SBRS=
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib /nologo /subsystem:console /debug /machine:I386
# ADD LINK32 kernel32.lib /nologo /subsystem:console /debug /machine:I386
LINK32_FLAGS=kernel32.lib /nologo /subsystem:console /incremental:yes\
 /pdb:"$(OUTDIR)/programa7_1.pdb" /debug /machine:I386\
 /out:"$(OUTDIR)/programa7_1.exe" 
LINK32_OBJS= \
	".\programa7_1\Debug\programa7_1.obj"

"$(OUTDIR)\programa7_1.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "programa7_1a - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "programa7_1a\Release"
# PROP BASE Intermediate_Dir "programa7_1a\Release"
# PROP BASE Target_Dir "programa7_1a"
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "programa7_1a\Release"
# PROP Intermediate_Dir "programa7_1a\Release"
# PROP Target_Dir "programa7_1a"
OUTDIR=.\programa7_1a\Release
INTDIR=.\programa7_1a\Release

ALL : "$(OUTDIR)\programa7_1a.exe"

CLEAN : 
	-@erase ".\programa7_1a\Release\programa7_1a.exe"
	-@erase ".\programa7_1a\Release\confereordem.obj"
	-@erase ".\programa7_1a\Release\programa7_1a.obj"
	-@erase ".\programa7_1a\Release\subrotina1.obj"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

# ADD BASE F90 /Ox /I "programa7_1a\Release/" /c /nologo
# ADD F90 /Ox /I "programa7_1a\Release/" /c /nologo
F90_PROJ=/Ox /I "programa7_1a\Release/" /c /nologo /Fo"programa7_1a\Release/" 
F90_OBJS=.\programa7_1a\Release/
# ADD BASE RSC /l 0x416 /d "NDEBUG"
# ADD RSC /l 0x416 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/programa7_1a.bsc" 
BSC32_SBRS=
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib /nologo /subsystem:console /machine:I386
# ADD LINK32 kernel32.lib /nologo /subsystem:console /machine:I386
LINK32_FLAGS=kernel32.lib /nologo /subsystem:console /incremental:no\
 /pdb:"$(OUTDIR)/programa7_1a.pdb" /machine:I386\
 /out:"$(OUTDIR)/programa7_1a.exe" 
LINK32_OBJS= \
	".\programa7_1a\Release\confereordem.obj" \
	".\programa7_1a\Release\programa7_1a.obj" \
	".\programa7_1a\Release\subrotina1.obj"

"$(OUTDIR)\programa7_1a.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "programa7_1a - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "programa7_1a\Debug"
# PROP BASE Intermediate_Dir "programa7_1a\Debug"
# PROP BASE Target_Dir "programa7_1a"
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "programa7_1a\Debug"
# PROP Intermediate_Dir "programa7_1a\Debug"
# PROP Target_Dir "programa7_1a"
OUTDIR=.\programa7_1a\Debug
INTDIR=.\programa7_1a\Debug

ALL : "$(OUTDIR)\programa7_1a.exe"

CLEAN : 
	-@erase ".\programa7_1a\Debug\programa7_1a.exe"
	-@erase ".\programa7_1a\Debug\confereordem.obj"
	-@erase ".\programa7_1a\Debug\subrotina1.obj"
	-@erase ".\programa7_1a\Debug\programa7_1a.obj"
	-@erase ".\programa7_1a\Debug\programa7_1a.ilk"
	-@erase ".\programa7_1a\Debug\programa7_1a.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

# ADD BASE F90 /Zi /I "programa7_1a\Debug/" /c /nologo
# ADD F90 /Zi /I "programa7_1a\Debug/" /c /nologo
F90_PROJ=/Zi /I "programa7_1a\Debug/" /c /nologo /Fo"programa7_1a\Debug/"\
 /Fd"programa7_1a\Debug/marchi.pdb" 
F90_OBJS=.\programa7_1a\Debug/
# ADD BASE RSC /l 0x416 /d "_DEBUG"
# ADD RSC /l 0x416 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/programa7_1a.bsc" 
BSC32_SBRS=
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib /nologo /subsystem:console /debug /machine:I386
# ADD LINK32 kernel32.lib /nologo /subsystem:console /debug /machine:I386
LINK32_FLAGS=kernel32.lib /nologo /subsystem:console /incremental:yes\
 /pdb:"$(OUTDIR)/programa7_1a.pdb" /debug /machine:I386\
 /out:"$(OUTDIR)/programa7_1a.exe" 
LINK32_OBJS= \
	".\programa7_1a\Debug\confereordem.obj" \
	".\programa7_1a\Debug\subrotina1.obj" \
	".\programa7_1a\Debug\programa7_1a.obj"

"$(OUTDIR)\programa7_1a.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "programa7_1b - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "programa7_1b\Release"
# PROP BASE Intermediate_Dir "programa7_1b\Release"
# PROP BASE Target_Dir "programa7_1b"
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "programa7_1b\Release"
# PROP Intermediate_Dir "programa7_1b\Release"
# PROP Target_Dir "programa7_1b"
OUTDIR=.\programa7_1b\Release
INTDIR=.\programa7_1b\Release

ALL : "$(OUTDIR)\programa7_1b.exe"

CLEAN : 
	-@erase ".\programa7_1b\Release\programa7_1b.exe"
	-@erase ".\programa7_1b\Release\programa7_1b.obj"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

# ADD BASE F90 /Ox /I "programa7_1b\Release/" /c /nologo
# ADD F90 /Ox /I "programa7_1b\Release/" /c /nologo
F90_PROJ=/Ox /I "programa7_1b\Release/" /c /nologo /Fo"programa7_1b\Release/" 
F90_OBJS=.\programa7_1b\Release/
# ADD BASE RSC /l 0x416 /d "NDEBUG"
# ADD RSC /l 0x416 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/programa7_1b.bsc" 
BSC32_SBRS=
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib /nologo /subsystem:console /machine:I386
# ADD LINK32 kernel32.lib /nologo /subsystem:console /machine:I386
LINK32_FLAGS=kernel32.lib /nologo /subsystem:console /incremental:no\
 /pdb:"$(OUTDIR)/programa7_1b.pdb" /machine:I386\
 /out:"$(OUTDIR)/programa7_1b.exe" 
LINK32_OBJS= \
	".\programa7_1b\Release\programa7_1b.obj"

"$(OUTDIR)\programa7_1b.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "programa7_1b - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "programa7_1b\Debug"
# PROP BASE Intermediate_Dir "programa7_1b\Debug"
# PROP BASE Target_Dir "programa7_1b"
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "programa7_1b\Debug"
# PROP Intermediate_Dir "programa7_1b\Debug"
# PROP Target_Dir "programa7_1b"
OUTDIR=.\programa7_1b\Debug
INTDIR=.\programa7_1b\Debug

ALL : "$(OUTDIR)\programa7_1b.exe"

CLEAN : 
	-@erase ".\programa7_1b\Debug\programa7_1b.exe"
	-@erase ".\programa7_1b\Debug\programa7_1b.obj"
	-@erase ".\programa7_1b\Debug\programa7_1b.ilk"
	-@erase ".\programa7_1b\Debug\programa7_1b.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

# ADD BASE F90 /Zi /I "programa7_1b\Debug/" /c /nologo
# ADD F90 /Zi /I "programa7_1b\Debug/" /c /nologo
F90_PROJ=/Zi /I "programa7_1b\Debug/" /c /nologo /Fo"programa7_1b\Debug/"\
 /Fd"programa7_1b\Debug/marchi.pdb" 
F90_OBJS=.\programa7_1b\Debug/
# ADD BASE RSC /l 0x416 /d "_DEBUG"
# ADD RSC /l 0x416 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/programa7_1b.bsc" 
BSC32_SBRS=
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib /nologo /subsystem:console /debug /machine:I386
# ADD LINK32 kernel32.lib /nologo /subsystem:console /debug /machine:I386
LINK32_FLAGS=kernel32.lib /nologo /subsystem:console /incremental:yes\
 /pdb:"$(OUTDIR)/programa7_1b.pdb" /debug /machine:I386\
 /out:"$(OUTDIR)/programa7_1b.exe" 
LINK32_OBJS= \
	".\programa7_1b\Debug\programa7_1b.obj"

"$(OUTDIR)\programa7_1b.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "programa8_1b - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "programa8_1b\Release"
# PROP BASE Intermediate_Dir "programa8_1b\Release"
# PROP BASE Target_Dir "programa8_1b"
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "programa8_1b\Release"
# PROP Intermediate_Dir "programa8_1b\Release"
# PROP Target_Dir "programa8_1b"
OUTDIR=.\programa8_1b\Release
INTDIR=.\programa8_1b\Release

ALL : "$(OUTDIR)\programa8_1b.exe"

CLEAN : 
	-@erase ".\programa8_1b\Release\programa8_1b.exe"
	-@erase ".\programa8_1b\Release\programa8_1b.obj"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

# ADD BASE F90 /Ox /I "programa8_1b\Release/" /c /nologo
# ADD F90 /Ox /I "programa8_1b\Release/" /c /nologo
F90_PROJ=/Ox /I "programa8_1b\Release/" /c /nologo /Fo"programa8_1b\Release/" 
F90_OBJS=.\programa8_1b\Release/
# ADD BASE RSC /l 0x416 /d "NDEBUG"
# ADD RSC /l 0x416 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/programa8_1b.bsc" 
BSC32_SBRS=
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib /nologo /subsystem:console /machine:I386
# ADD LINK32 kernel32.lib /nologo /subsystem:console /machine:I386
LINK32_FLAGS=kernel32.lib /nologo /subsystem:console /incremental:no\
 /pdb:"$(OUTDIR)/programa8_1b.pdb" /machine:I386\
 /out:"$(OUTDIR)/programa8_1b.exe" 
LINK32_OBJS= \
	".\programa8_1b\Release\programa8_1b.obj"

"$(OUTDIR)\programa8_1b.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "programa8_1b - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "programa8_1b\Debug"
# PROP BASE Intermediate_Dir "programa8_1b\Debug"
# PROP BASE Target_Dir "programa8_1b"
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "programa8_1b\Debug"
# PROP Intermediate_Dir "programa8_1b\Debug"
# PROP Target_Dir "programa8_1b"
OUTDIR=.\programa8_1b\Debug
INTDIR=.\programa8_1b\Debug

ALL : "$(OUTDIR)\programa8_1b.exe"

CLEAN : 
	-@erase ".\programa8_1b\Debug\programa8_1b.exe"
	-@erase ".\programa8_1b\Debug\programa8_1b.obj"
	-@erase ".\programa8_1b\Debug\programa8_1b.ilk"
	-@erase ".\programa8_1b\Debug\programa8_1b.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

# ADD BASE F90 /Zi /I "programa8_1b\Debug/" /c /nologo
# ADD F90 /Zi /I "programa8_1b\Debug/" /c /nologo
F90_PROJ=/Zi /I "programa8_1b\Debug/" /c /nologo /Fo"programa8_1b\Debug/"\
 /Fd"programa8_1b\Debug/marchi.pdb" 
F90_OBJS=.\programa8_1b\Debug/
# ADD BASE RSC /l 0x416 /d "_DEBUG"
# ADD RSC /l 0x416 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/programa8_1b.bsc" 
BSC32_SBRS=
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib /nologo /subsystem:console /debug /machine:I386
# ADD LINK32 kernel32.lib /nologo /subsystem:console /debug /machine:I386
LINK32_FLAGS=kernel32.lib /nologo /subsystem:console /incremental:yes\
 /pdb:"$(OUTDIR)/programa8_1b.pdb" /debug /machine:I386\
 /out:"$(OUTDIR)/programa8_1b.exe" 
LINK32_OBJS= \
	".\programa8_1b\Debug\programa8_1b.obj"

"$(OUTDIR)\programa8_1b.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "programa8_6 - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "programa8_6\Release"
# PROP BASE Intermediate_Dir "programa8_6\Release"
# PROP BASE Target_Dir "programa8_6"
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "programa8_6\Release"
# PROP Intermediate_Dir "programa8_6\Release"
# PROP Target_Dir "programa8_6"
OUTDIR=.\programa8_6\Release
INTDIR=.\programa8_6\Release

ALL : "$(OUTDIR)\programa8_6.exe"

CLEAN : 
	-@erase ".\programa8_6\Release\programa8_6.exe"
	-@erase ".\programa8_6\Release\programa8_6.obj"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

# ADD BASE F90 /Ox /I "programa8_6\Release/" /c /nologo
# ADD F90 /Ox /I "programa8_6\Release/" /c /nologo
F90_PROJ=/Ox /I "programa8_6\Release/" /c /nologo /Fo"programa8_6\Release/" 
F90_OBJS=.\programa8_6\Release/
# ADD BASE RSC /l 0x416 /d "NDEBUG"
# ADD RSC /l 0x416 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/programa8_6.bsc" 
BSC32_SBRS=
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib /nologo /subsystem:console /machine:I386
# ADD LINK32 kernel32.lib /nologo /subsystem:console /machine:I386
LINK32_FLAGS=kernel32.lib /nologo /subsystem:console /incremental:no\
 /pdb:"$(OUTDIR)/programa8_6.pdb" /machine:I386 /out:"$(OUTDIR)/programa8_6.exe"\
 
LINK32_OBJS= \
	".\programa8_6\Release\programa8_6.obj"

"$(OUTDIR)\programa8_6.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "programa8_6 - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "programa8_6\Debug"
# PROP BASE Intermediate_Dir "programa8_6\Debug"
# PROP BASE Target_Dir "programa8_6"
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "programa8_6\Debug"
# PROP Intermediate_Dir "programa8_6\Debug"
# PROP Target_Dir "programa8_6"
OUTDIR=.\programa8_6\Debug
INTDIR=.\programa8_6\Debug

ALL : "$(OUTDIR)\programa8_6.exe"

CLEAN : 
	-@erase ".\programa8_6\Debug\programa8_6.exe"
	-@erase ".\programa8_6\Debug\programa8_6.obj"
	-@erase ".\programa8_6\Debug\programa8_6.ilk"
	-@erase ".\programa8_6\Debug\programa8_6.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

# ADD BASE F90 /Zi /I "programa8_6\Debug/" /c /nologo
# ADD F90 /Zi /I "programa8_6\Debug/" /c /nologo
F90_PROJ=/Zi /I "programa8_6\Debug/" /c /nologo /Fo"programa8_6\Debug/"\
 /Fd"programa8_6\Debug/marchi.pdb" 
F90_OBJS=.\programa8_6\Debug/
# ADD BASE RSC /l 0x416 /d "_DEBUG"
# ADD RSC /l 0x416 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/programa8_6.bsc" 
BSC32_SBRS=
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib /nologo /subsystem:console /debug /machine:I386
# ADD LINK32 kernel32.lib /nologo /subsystem:console /debug /machine:I386
LINK32_FLAGS=kernel32.lib /nologo /subsystem:console /incremental:yes\
 /pdb:"$(OUTDIR)/programa8_6.pdb" /debug /machine:I386\
 /out:"$(OUTDIR)/programa8_6.exe" 
LINK32_OBJS= \
	".\programa8_6\Debug\programa8_6.obj"

"$(OUTDIR)\programa8_6.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "programa8_2 - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "programa8_2\Release"
# PROP BASE Intermediate_Dir "programa8_2\Release"
# PROP BASE Target_Dir "programa8_2"
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "programa8_2\Release"
# PROP Intermediate_Dir "programa8_2\Release"
# PROP Target_Dir "programa8_2"
OUTDIR=.\programa8_2\Release
INTDIR=.\programa8_2\Release

ALL : "$(OUTDIR)\programa8_2.exe"

CLEAN : 
	-@erase ".\programa8_2\Release\programa8_2.exe"
	-@erase ".\programa8_2\Release\programa8_2.obj"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

# ADD BASE F90 /Ox /I "programa8_2\Release/" /c /nologo
# ADD F90 /Ox /I "programa8_2\Release/" /c /nologo
F90_PROJ=/Ox /I "programa8_2\Release/" /c /nologo /Fo"programa8_2\Release/" 
F90_OBJS=.\programa8_2\Release/
# ADD BASE RSC /l 0x416 /d "NDEBUG"
# ADD RSC /l 0x416 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/programa8_2.bsc" 
BSC32_SBRS=
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib /nologo /subsystem:console /machine:I386
# ADD LINK32 kernel32.lib /nologo /subsystem:console /machine:I386
LINK32_FLAGS=kernel32.lib /nologo /subsystem:console /incremental:no\
 /pdb:"$(OUTDIR)/programa8_2.pdb" /machine:I386 /out:"$(OUTDIR)/programa8_2.exe"\
 
LINK32_OBJS= \
	".\programa8_2\Release\programa8_2.obj"

"$(OUTDIR)\programa8_2.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "programa8_2 - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "programa8_2\Debug"
# PROP BASE Intermediate_Dir "programa8_2\Debug"
# PROP BASE Target_Dir "programa8_2"
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "programa8_2\Debug"
# PROP Intermediate_Dir "programa8_2\Debug"
# PROP Target_Dir "programa8_2"
OUTDIR=.\programa8_2\Debug
INTDIR=.\programa8_2\Debug

ALL : "$(OUTDIR)\programa8_2.exe"

CLEAN : 
	-@erase ".\programa8_2\Debug\programa8_2.exe"
	-@erase ".\programa8_2\Debug\programa8_2.obj"
	-@erase ".\programa8_2\Debug\programa8_2.ilk"
	-@erase ".\programa8_2\Debug\programa8_2.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

# ADD BASE F90 /Zi /I "programa8_2\Debug/" /c /nologo
# ADD F90 /Zi /I "programa8_2\Debug/" /c /nologo
F90_PROJ=/Zi /I "programa8_2\Debug/" /c /nologo /Fo"programa8_2\Debug/"\
 /Fd"programa8_2\Debug/marchi.pdb" 
F90_OBJS=.\programa8_2\Debug/
# ADD BASE RSC /l 0x416 /d "_DEBUG"
# ADD RSC /l 0x416 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/programa8_2.bsc" 
BSC32_SBRS=
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib /nologo /subsystem:console /debug /machine:I386
# ADD LINK32 kernel32.lib /nologo /subsystem:console /debug /machine:I386
LINK32_FLAGS=kernel32.lib /nologo /subsystem:console /incremental:yes\
 /pdb:"$(OUTDIR)/programa8_2.pdb" /debug /machine:I386\
 /out:"$(OUTDIR)/programa8_2.exe" 
LINK32_OBJS= \
	".\programa8_2\Debug\programa8_2.obj"

"$(OUTDIR)\programa8_2.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "programa8_3 - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "programa8_3\Release"
# PROP BASE Intermediate_Dir "programa8_3\Release"
# PROP BASE Target_Dir "programa8_3"
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "programa8_3\Release"
# PROP Intermediate_Dir "programa8_3\Release"
# PROP Target_Dir "programa8_3"
OUTDIR=.\programa8_3\Release
INTDIR=.\programa8_3\Release

ALL : "$(OUTDIR)\programa8_3.exe"

CLEAN : 
	-@erase ".\programa8_3\Release\programa8_3.exe"
	-@erase ".\programa8_3\Release\programa8_3.obj"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

# ADD BASE F90 /Ox /I "programa8_3\Release/" /c /nologo
# ADD F90 /Ox /I "programa8_3\Release/" /c /nologo
F90_PROJ=/Ox /I "programa8_3\Release/" /c /nologo /Fo"programa8_3\Release/" 
F90_OBJS=.\programa8_3\Release/
# ADD BASE RSC /l 0x416 /d "NDEBUG"
# ADD RSC /l 0x416 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/programa8_3.bsc" 
BSC32_SBRS=
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib /nologo /subsystem:console /machine:I386
# ADD LINK32 kernel32.lib /nologo /subsystem:console /machine:I386
LINK32_FLAGS=kernel32.lib /nologo /subsystem:console /incremental:no\
 /pdb:"$(OUTDIR)/programa8_3.pdb" /machine:I386 /out:"$(OUTDIR)/programa8_3.exe"\
 
LINK32_OBJS= \
	".\programa8_3\Release\programa8_3.obj"

"$(OUTDIR)\programa8_3.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "programa8_3 - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "programa8_3\Debug"
# PROP BASE Intermediate_Dir "programa8_3\Debug"
# PROP BASE Target_Dir "programa8_3"
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "programa8_3\Debug"
# PROP Intermediate_Dir "programa8_3\Debug"
# PROP Target_Dir "programa8_3"
OUTDIR=.\programa8_3\Debug
INTDIR=.\programa8_3\Debug

ALL : "$(OUTDIR)\programa8_3.exe"

CLEAN : 
	-@erase ".\programa8_3\Debug\programa8_3.exe"
	-@erase ".\programa8_3\Debug\programa8_3.obj"
	-@erase ".\programa8_3\Debug\programa8_3.ilk"
	-@erase ".\programa8_3\Debug\programa8_3.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

# ADD BASE F90 /Zi /I "programa8_3\Debug/" /c /nologo
# ADD F90 /Zi /I "programa8_3\Debug/" /c /nologo
F90_PROJ=/Zi /I "programa8_3\Debug/" /c /nologo /Fo"programa8_3\Debug/"\
 /Fd"programa8_3\Debug/marchi.pdb" 
F90_OBJS=.\programa8_3\Debug/
# ADD BASE RSC /l 0x416 /d "_DEBUG"
# ADD RSC /l 0x416 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/programa8_3.bsc" 
BSC32_SBRS=
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib /nologo /subsystem:console /debug /machine:I386
# ADD LINK32 kernel32.lib /nologo /subsystem:console /debug /machine:I386
LINK32_FLAGS=kernel32.lib /nologo /subsystem:console /incremental:yes\
 /pdb:"$(OUTDIR)/programa8_3.pdb" /debug /machine:I386\
 /out:"$(OUTDIR)/programa8_3.exe" 
LINK32_OBJS= \
	".\programa8_3\Debug\programa8_3.obj"

"$(OUTDIR)\programa8_3.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ENDIF 

.for{$(F90_OBJS)}.obj:
   $(F90) $(F90_PROJ) $<  

.f{$(F90_OBJS)}.obj:
   $(F90) $(F90_PROJ) $<  

.f90{$(F90_OBJS)}.obj:
   $(F90) $(F90_PROJ) $<  

################################################################################
# Begin Target

# Name "marchi - Win32 Release"
# Name "marchi - Win32 Debug"

!IF  "$(CFG)" == "marchi - Win32 Release"

!ELSEIF  "$(CFG)" == "marchi - Win32 Debug"

!ENDIF 

# End Target
################################################################################
# Begin Target

# Name "programa12_1 - Win32 Release"
# Name "programa12_1 - Win32 Debug"

!IF  "$(CFG)" == "programa12_1 - Win32 Release"

!ELSEIF  "$(CFG)" == "programa12_1 - Win32 Debug"

!ENDIF 

################################################################################
# Begin Source File

SOURCE=.\programa12_1\programa12_1.f90

"$(INTDIR)\programa12_1.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


# End Source File
# End Target
################################################################################
# Begin Target

# Name "programa7_1 - Win32 Release"
# Name "programa7_1 - Win32 Debug"

!IF  "$(CFG)" == "programa7_1 - Win32 Release"

!ELSEIF  "$(CFG)" == "programa7_1 - Win32 Debug"

!ENDIF 

################################################################################
# Begin Source File

SOURCE=.\programa7_1\programa7_1.f90

"$(INTDIR)\programa7_1.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


# End Source File
# End Target
################################################################################
# Begin Target

# Name "programa7_1a - Win32 Release"
# Name "programa7_1a - Win32 Debug"

!IF  "$(CFG)" == "programa7_1a - Win32 Release"

!ELSEIF  "$(CFG)" == "programa7_1a - Win32 Debug"

!ENDIF 

################################################################################
# Begin Source File

SOURCE=.\programa7_1a\programa7_1a

!IF  "$(CFG)" == "programa7_1a - Win32 Release"

!ELSEIF  "$(CFG)" == "programa7_1a - Win32 Debug"

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\programa7_1a\programa7_1a.f90

"$(INTDIR)\programa7_1a.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=.\programa7_1a\subrotina1.f90

"$(INTDIR)\subrotina1.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


# End Source File
################################################################################
# Begin Source File

SOURCE=.\programa7_1a\confereordem.f90

"$(INTDIR)\confereordem.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


# End Source File
# End Target
################################################################################
# Begin Target

# Name "programa7_1b - Win32 Release"
# Name "programa7_1b - Win32 Debug"

!IF  "$(CFG)" == "programa7_1b - Win32 Release"

!ELSEIF  "$(CFG)" == "programa7_1b - Win32 Debug"

!ENDIF 

################################################################################
# Begin Source File

SOURCE=.\programa7_1b\programa7_1b.f90

"$(INTDIR)\programa7_1b.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


# End Source File
# End Target
################################################################################
# Begin Target

# Name "programa8_1b - Win32 Release"
# Name "programa8_1b - Win32 Debug"

!IF  "$(CFG)" == "programa8_1b - Win32 Release"

!ELSEIF  "$(CFG)" == "programa8_1b - Win32 Debug"

!ENDIF 

################################################################################
# Begin Source File

SOURCE=.\programa8_1b\programa8_1b

!IF  "$(CFG)" == "programa8_1b - Win32 Release"

!ELSEIF  "$(CFG)" == "programa8_1b - Win32 Debug"

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=.\programa8_1b\programa8_1b.f90

"$(INTDIR)\programa8_1b.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


# End Source File
# End Target
################################################################################
# Begin Target

# Name "programa8_6 - Win32 Release"
# Name "programa8_6 - Win32 Debug"

!IF  "$(CFG)" == "programa8_6 - Win32 Release"

!ELSEIF  "$(CFG)" == "programa8_6 - Win32 Debug"

!ENDIF 

################################################################################
# Begin Source File

SOURCE=.\programa8_6\programa8_6.f90

"$(INTDIR)\programa8_6.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


# End Source File
# End Target
################################################################################
# Begin Target

# Name "programa8_2 - Win32 Release"
# Name "programa8_2 - Win32 Debug"

!IF  "$(CFG)" == "programa8_2 - Win32 Release"

!ELSEIF  "$(CFG)" == "programa8_2 - Win32 Debug"

!ENDIF 

################################################################################
# Begin Source File

SOURCE=.\programa8_2\programa8_2.f90

"$(INTDIR)\programa8_2.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


# End Source File
# End Target
################################################################################
# Begin Target

# Name "programa8_3 - Win32 Release"
# Name "programa8_3 - Win32 Debug"

!IF  "$(CFG)" == "programa8_3 - Win32 Release"

!ELSEIF  "$(CFG)" == "programa8_3 - Win32 Debug"

!ENDIF 

################################################################################
# Begin Source File

SOURCE=.\programa8_3\programa8_3.f90

"$(INTDIR)\programa8_3.obj" : $(SOURCE) "$(INTDIR)"
   $(F90) $(F90_PROJ) $(SOURCE)


# End Source File
# End Target
# End Project
################################################################################
