UseSQLiteDatabase()

Structure Note
  Type.i
  Pitch.i
  StartStop.i
EndStructure

Structure NotePoint
  Array Notes.Note(0)
EndStructure

Global Dim Track.NotePoint(0)  

Enumeration WinMain
  #WinMain
  #cmbBPM
  #txtBPM
  #txtTrackLength
  #strLength
  #txtVolume
  #cmbVolume
  #btnPlay
  #cmbInstrumentGroup
  #cmbInstrument
  #txtInstrument
  #cnvTrack
  #imgTrack
  #mnuNotes
EndEnumeration

Global MusicDB.i,Instrument.i,Volume.i,Playing.i

Global BaseImage.i,TempTime.i,TempPitch.i

Procedure CheckData()
  
  ;Start
  ReDim Track(0)\Notes(2)
  Track(0)\Notes(0)\Pitch = 60         ;Middle C
  Track(0)\Notes(0)\Type = 1           ;Quarter Note
  Track(0)\Notes(0)\StartStop = #True  ;Start The Note
  Track(0)\Notes(1)\Pitch = 65         ;
  Track(0)\Notes(1)\Type = 4           ;Quarter Note
  Track(0)\Notes(1)\StartStop = #True  ;Start The Note 
  
  ;1/8
  Track(1)\Notes(0)\Pitch = 0          ;
  Track(1)\Notes(0)\Type = 0           ;
  Track(1)\Notes(0)\StartStop = #False ; 
  
  ;2/8
  ReDim Track(2)\Notes(2)
  Track(2)\Notes(0)\Pitch = 60         ;Middle C
  Track(2)\Notes(0)\Type = 1           ;Quarter Note
  Track(2)\Notes(0)\StartStop = #False ;Stop The Note
  Track(2)\Notes(1)\Pitch = 0         ;Middle C
  Track(2)\Notes(1)\Type = 1           ;Quarter Note
  Track(2)\Notes(1)\StartStop = #True  ;Start The Note  
  
  ;3/8
  Track(3)\Notes(0)\Pitch = 72          ;Nothing
  Track(3)\Notes(0)\Type = 1           ;
  Track(3)\Notes(0)\StartStop = #True ;   
  
  ;4/8
  ReDim Track(4)\Notes(2) 
  Track(4)\Notes(0)\Pitch = 72         ;
  Track(4)\Notes(0)\Type = 1           ;
  Track(4)\Notes(0)\StartStop = #False ;  
  Track(4)\Notes(1)\Pitch = 65         ;
  Track(4)\Notes(1)\Type = 4           ;Quarter Note
  Track(4)\Notes(1)\StartStop = #False 
  
  
EndProcedure

Procedure OpenMusicDB()
  
  MusicDB = OpenDatabase(#PB_Any, GetCurrentDirectory() + "Midi.db", "", "")   
  
EndProcedure

Procedure AddTempo()
  
  AddGadgetItem(#cmbBPM,-1,"60")
  AddGadgetItem(#cmbBPM,-1,"120")  
  SetGadgetText(#cmbBPM,"60")
  
EndProcedure

Procedure AddVolume()
  
  AddGadgetItem(#cmbVolume,-1,"ppp") ;16
  AddGadgetItem(#cmbVolume,-1,"pp")  ;33
  AddGadgetItem(#cmbVolume,-1,"p") ;49
  AddGadgetItem(#cmbVolume,-1,"mp")  ;64
  AddGadgetItem(#cmbVolume,-1,"mf");80
  AddGadgetItem(#cmbVolume,-1,"f")  ;96
  AddGadgetItem(#cmbVolume,-1,"ff");112
  AddGadgetItem(#cmbVolume,-1,"fff")  ;126
  SetGadgetText(#cmbVolume,"fff")
  
EndProcedure

Procedure AddInstrumentGroups()
  
  Define Criteria.s
  Define iLoop.i
  
  Criteria = "SELECT * FROM InstrumentGroups ORDER BY Name ASC;"
  
  If DatabaseQuery(MusicDB, Criteria) 
    iLoop = 0
    While NextDatabaseRow(MusicDB) ; Loop for each records
      AddGadgetItem(#cmbInstrumentGroup,iLoop,GetDatabaseString(MusicDB, 1)) 
      SetGadgetItemData(#cmbInstrumentGroup,iLoop,GetDatabaseLong(MusicDB, 0))
      iLoop = iLoop + 1
    Wend
  
    FinishDatabaseQuery(MusicDB)
  Else
    Debug DatabaseError()
  EndIf

EndProcedure

Procedure AddInstruments(InstrumentID.i)
  
  Define Criteria.s
  Define iLoop.i
  
  ClearGadgetItems(#cmbInstrument)
    
  Criteria = "SELECT * FROM Instruments WHERE GroupID = " + Str(InstrumentID) + " ORDER BY Name ASC;"
  
  If DatabaseQuery(MusicDB, Criteria) 
    iLoop = 0
    While NextDatabaseRow(MusicDB) ; Loop for each records
      AddGadgetItem(#cmbInstrument,iLoop,GetDatabaseString(MusicDB, 2)) 
      SetGadgetItemData(#cmbInstrument,iLoop,GetDatabaseLong(MusicDB, 1))
      iLoop = iLoop + 1
    Wend
  
    FinishDatabaseQuery(MusicDB)
  Else
    Debug DatabaseError()
  EndIf
  
EndProcedure

Global hMidiOut

Global midi.MIDIOUTCAPS
Global devices = midiOutGetNumDevs_()

For devnum=-1 To devices-1
  If midiOutGetDevCaps_(devnum,@midi,SizeOf(MIDIOUTCAPS))=0
    If midi\wVoices >0
      Global midiport=devnum
    EndIf
  EndIf
Next

Procedure MidiOutMessage(hMidi,iStatus,iChannel,iData1,iData2)
  dwMessage = iStatus | iChannel | (iData1 << 8 ) | (iData2 << 16)
  ProcedureReturn midiOutShortMsg_(hMidi, dwMessage) ;
EndProcedure
 
Procedure SetInstrument(channel,instrument)
  MidiOutMessage(hMidiOut, $C0,  channel, instrument, 0)
EndProcedure
 
Procedure PlayNote(channel,Note,velocity)
  MidiOutMessage(hMidiOut, $90, channel, Note , velocity)
EndProcedure

Procedure StopNote(channel,Note)
  MidiOutMessage(hMidiOut, $90, channel, Note , 0)
EndProcedure

Procedure MIDIVolume(Channel,Volume)
  midiOutShortMsg_(hMidiOut,$B0 | Channel | $700 | Volume << 16 )
EndProcedure

Procedure AddNote(TimeIndex.i,Type.i,Pitch.i)

  iLoop = ArraySize(Track(TimeIndex)\Notes())

  ;Add Start Of Note
  Track(TimeIndex)\Notes(iLoop)\Pitch = Pitch
  Track(TimeIndex)\Notes(iLoop)\StartStop = #True
  Track(TimeIndex)\Notes(iLoop)\Type = Type
  ReDim Track(TimeIndex)\Notes(iLoop + 1)
  
  ;Add End Of Note
  TimeIndex = TimeIndex + Type
  If TimeIndex > ArraySize(Track())
    TimeIndex = ArraySize(Track())-1
  EndIf
  
  iLoop = ArraySize(Track(TimeIndex)\Notes())

  Track(TimeIndex)\Notes(iLoop)\Pitch = Pitch
  Track(TimeIndex)\Notes(iLoop)\StartStop = #False
  Track(TimeIndex)\Notes(iLoop)\Type = Type
  ReDim Track(TimeIndex)\Notes(iLoop + 1)
    
EndProcedure

Procedure DrawBackImage()
  
  BaseImage = CreateImage(#PB_Any,500,400,32,RGBA(255,255,255,0))
  
  StartVectorDrawing(ImageVectorOutput(BaseImage))
  
  VectorSourceColor(RGBA(0,255,255,255))
  MovePathCursor(0,20)
  AddPathLine(512,20)
  MovePathCursor(0,40)
  AddPathLine(512,40)  
  MovePathCursor(0,60)
  AddPathLine(512,60)
  MovePathCursor(0,80)
  AddPathLine(512,80)  
  MovePathCursor(0,100)
  AddPathLine(512,100)  
  MovePathCursor(0,120)
  AddPathLine(512,120)
  MovePathCursor(0,140)
  AddPathLine(512,140)
  MovePathCursor(0,160)
  AddPathLine(512,160)  
  MovePathCursor(0,180)
  AddPathLine(512,180)
  MovePathCursor(0,200)
  AddPathLine(512,200)  
  MovePathCursor(0,220)
  AddPathLine(512,220)
  MovePathCursor(0,240)
  AddPathLine(512,240)  
  MovePathCursor(0,260)
  AddPathLine(512,260)
  MovePathCursor(0,280)
  AddPathLine(512,280)  
  MovePathCursor(0,300)
  AddPathLine(512,300)  
  MovePathCursor(0,320)
  AddPathLine(512,320)
  MovePathCursor(0,340)
  AddPathLine(512,340)
  MovePathCursor(0,360)
  AddPathLine(512,360)  
  MovePathCursor(0,380)
  AddPathLine(512,380)
  StrokePath(0.5)

  VectorSourceColor(RGBA(255,255,0,255))
  MovePathCursor(20,0)
  AddPathLine(20,400)
  MovePathCursor(40,0)
  AddPathLine(40,400) 
  MovePathCursor(60,0)
  AddPathLine(60,400)
  MovePathCursor(80,0)
  AddPathLine(80,400)   
  MovePathCursor(100,0)
  AddPathLine(100,400)
  MovePathCursor(120,0)
  AddPathLine(120,400) 
  MovePathCursor(140,0)
  AddPathLine(140,400)
  MovePathCursor(160,0)
  AddPathLine(160,400)  
  MovePathCursor(180,0)
  AddPathLine(180,400)
  MovePathCursor(200,0)
  AddPathLine(200,400) 
  MovePathCursor(220,0)
  AddPathLine(220,400)
  MovePathCursor(240,0)
  AddPathLine(240,400)   
  MovePathCursor(260,0)
  AddPathLine(260,400)
  MovePathCursor(280,0)
  AddPathLine(280,400) 
  MovePathCursor(300,0)
  AddPathLine(300,400)
  MovePathCursor(320,0) 
  AddPathLine(320,400)
  MovePathCursor(340,0)
  AddPathLine(340,400)   
  MovePathCursor(360,0)
  AddPathLine(360,400)
  MovePathCursor(380,0)
  AddPathLine(380,400) 
  MovePathCursor(400,0)
  AddPathLine(400,400) 
  MovePathCursor(420,0) 
  AddPathLine(420,400)
  MovePathCursor(440,0)
  AddPathLine(440,400)   
  MovePathCursor(460,0)
  AddPathLine(460,400)
  MovePathCursor(480,0)
  AddPathLine(480,400)   
  StrokePath(1)

  StopVectorDrawing()
  
EndProcedure

Procedure ShowNotes(ThisTrack.i)
  
  Define iLoop.i,nLoop.i
  
  StartVectorDrawing(CanvasVectorOutput(#cnvTrack))
  
    ;Clear To Base Image
    DrawVectorImage(ImageID(BaseImage),255)
    
    VectorSourceColor(RGBA(255,0,0,255))
    For iLoop = Trackposition To Trackposition + 20
      If iLoop > ArraySize(Track())
        Break
      EndIf
      
      For nLoop  = 0 To ArraySize(Track(iLoop)\Notes())
        If Track(iLoop)\Notes(nLoop)\Pitch > 0  And Track(iLoop)\Notes(nLoop)\StartStop = #True 
          MovePathCursor(iLoop * 20,(380 - (Track(iLoop)\Notes(nLoop)\Pitch-60) * 20) + 10)
          AddPathLine((iLoop * 20) + (Track(iLoop)\Notes(nLoop)\Type * 20),(380 - (Track(iLoop)\Notes(nLoop)\Pitch-60) * 20)+10);(400 - (Track(iLoop)\Notes(nLoop)\Pitch-60) * 20)) * Track(iLoop)\Notes(nLoop)\Type
        EndIf
      Next nLoop
    Next iLoop  
    StrokePath(10)  
    
    If Playing = #True
      VectorSourceColor(RGBA(0,0,255,255))
      MovePathCursor(ThisTrack * 20,0)
      AddPathLine(ThisTrack * 20,400)
      StrokePath(1)
    EndIf
    
    StopVectorDrawing()
  
EndProcedure

Procedure PlayProc(Trackposition.i)
  
  ;For each note at this point
  For iLoop  = 0 To ArraySize(Track(Trackposition)\Notes())
    If Track(Trackposition)\Notes(iLoop)\Pitch > 0
      If Track(Trackposition)\Notes(iLoop)\StartStop = #True
        ;Start Playing This Note
         PlayNote(1, Track(Trackposition)\Notes(iLoop)\Pitch, 127)       
      Else
        ;Stop playing this note
        StopNote(1,Track(Trackposition)\Notes(iLoop)\Pitch)
      EndIf
    EndIf  
  Next
  
  ShowNotes(Trackposition)
  
EndProcedure

Procedure ActionTimer(TrackLength.i)
  
  Define s.i,t.i
    Define position.i = 0
    t = ElapsedMilliseconds()
    Repeat
      s = ElapsedMilliseconds()
      If s-t => 125 ;1/8 of a second
        PlayProc(position)
        t = s
        position = position + 1       
      EndIf
      Delay(1) ;keep cpu use in check
    Until position = TrackLength

EndProcedure

Window_0 = OpenWindow(#WinMain, 0, 0, 600, 500, "", #PB_Window_SystemMenu)
TextGadget(#txtTrackLength, 20, 10, 60, 20, "Length")
StringGadget(#strLength, 20, 40, 60, 20, "")
TextGadget(#txtBPM, 90, 10, 50, 20, "Tempo")
ComboBoxGadget(#cmbBPM, 90, 40, 50, 20)
TextGadget(#txtVolume, 150, 10, 50, 20, "Volume")
ComboBoxGadget(#cmbVolume, 150, 40, 50, 20)
TextGadget(#txtInstrument, 210, 10, 90, 20, "Instrument")
ComboBoxGadget(#cmbInstrumentGroup, 210, 40, 120, 20)
ComboBoxGadget(#cmbInstrument, 340, 40, 120, 20)
CanvasGadget(#cnvTrack, 10, 70, 500, 400)
ButtonGadget(#btnPlay, 510, 30, 70, 30, "Play")


ReDim Track((3 * 8)+1)  ;Seconds * 8
CheckData()

If CreatePopupMenu(#mnuNotes)
  OpenSubMenu("Add Note")
    MenuItem(1, "Quaver")
    MenuItem(2, "Crochet")
    MenuItem(3, "Minim")    
    MenuItem(4, "Semibreve")    
  CloseSubMenu()  
  MenuItem(5, "Delete")
EndIf

OpenMusicDB()
AddTempo()
AddVolume()
AddInstrumentGroups()
Playing = #False
DrawBackImage()
StartVectorDrawing(CanvasVectorOutput(#cnvTrack))
DrawVectorImage(ImageID(BaseImage),255)
StopVectorDrawing()

ShowNotes(0)

Repeat
   
  Event = WaitWindowEvent()
  
  Select event
      
    Case #PB_Event_Menu  
      
      Select EventMenu()
          
        Case 1
          
          ;Quaver          
          AddNote(TempTime,1,TempPitch)
          ShowNotes(0)
          
        Case 2
          
          ;Crochet
          AddNote(TempTime,2,TempPitch)
          ShowNotes(0)
          
        Case 3
          
          ;Minim
          AddNote(TempTime,4,TempPitch)
          ShowNotes(0)
          
        Case 4
          
          ;Semibreve
          AddNote(TempTime,8,TempPitch)
          ShowNotes(0) 
          
        Case 5
          
          Debug "Delete"
          
      EndSelect
      
    Case #PB_Event_Gadget
      
      Select EventGadget()
          
        Case #cmbInstrumentGroup

          AddInstruments(GetGadgetItemData(#cmbInstrumentGroup,GetGadgetState(#cmbInstrumentGroup)))
          
        Case #cmbInstrument   
          
          Instrument = GetGadgetItemData(#cmbInstrument,GetGadgetState(#cmbInstrument))
          
        Case #btnPlay
          midiOutOpen_(@hMidiOut,midiport,0,0,0)
          Playing = #True
          ;Set Instrument          
          SetInstrument(1,Instrument)
          ;Reset to start of track
          Trackposition = 0
          ;Start Play Timer
          Thread = CreateThread(@ActionTimer(), 25)
          While IsThread(Thread)
            Delay (500) ;Thread time = 125 so allow to finish correctly
          Wend
          Playing = #False
          midiOutClose_(hMidiOut)
          ShowNotes(0)
          
        Case #cnvTrack
          
          Select EventType()
          
            Case #PB_EventType_RightClick
              XPos = GetGadgetAttribute(#cnvTrack,#PB_Canvas_MouseX)/20
              TempTime = XPos
              
              YPos = GetGadgetAttribute(#cnvTrack,#PB_Canvas_MouseY)/20
              TempPitch = 80 -(YPos + 1) 
              
              DisplayPopupMenu(#mnuNotes, WindowID(#WinMain))

        EndSelect
          
      EndSelect
     
  EndSelect
 
Until Event = #PB_Event_CloseWindow
; IDE Options = PureBasic 5.51 (Windows - x64)
; CursorPosition = 502
; Folding = AAw
; EnableXP