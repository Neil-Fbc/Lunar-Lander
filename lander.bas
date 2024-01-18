' Compile fbc -mt lander.bas
' LANDER by redcrab version 1.4.1
' FreeBASIC code to be compile with FBC v 0.17  http://www.freebasic.net
' 2007 redcrab@infonie.fr http://csgp.suret.net

'********************
'      Sound FX
'********************
' IF NO_SOUND all sound are played but silently(No lib dependency !)
#Define SOUNDLIB NO_SOUND
'#Define SOUNDLIB FBSOUND
' ---------------------
' ----- DECLARATION


#If SOUNDLIB=FBSOUND
#Include "fbsound-1.2/inc/fbsound_dynamic.bi"
#Define SOUNDLIBNAME "FBSOUND"
fbs_Set_PlugPath("fbsound-1.2/inc/")
#EndIf

#If SOUNDLIB=NO_SOUND
#Define SOUNDLIBNAME "NO_SOUND"
#endif

#Ifndef PI
#Define PI 3.141592654
#EndIf
' wave form sample quantity 
#If SOUNDLIB=FBSOUND
#Define RS_SAMPLESIZE 1600
#Define RS_SAMPLEPERBUFFER 10
#EndIf

#If SOUNDLIB=NO_SOUND
#Define RS_SAMPLESIZE 16
#Define RS_SAMPLEPERBUFFER 1
#EndIf

' wave form catalog size
#Define RS_MAXWAVEFORM 15
#Define RS_MINWAVEFORM 0
' channel quantity
#Define RS_MAXCHANNEL 7 
#Define RS_MINCHANNEL 0 
' Note Buffer
#Define RS_MINNOTEBUFFER 0
#Define RS_MAXNOTEBUFFER 6000

Enum waveform
  SINE = 0
  SAW
  SQUARE
  SPECIAL
  NOISE = RS_MAXWAVEFORM
End Enum


' Channel Information 
Type RS_Channel
	channel As Integer
	timestart As Double
	delay As Double
	wfidx As Integer
	frequency As Integer
	volume As Integer
	isOn As integer
	Declare Constructor()
End Type

Type RS_Note
	waveform As Integer
	frequency As Integer
	volume As Integer
	delay As Double
End Type


Type RS_NoteBuffer
	_noteBuffer(RS_MINNOTEBUFFER To RS_MAXNOTEBUFFER) As RS_Note
	_noteBufferStart As Integer
	_noteBufferEnd As Integer
	Declare Sub addNote(wfidx As Integer, freq As Integer, vol As integer, delay As Double)
	Declare Function getNote() As RS_Note
	Declare Function hasNote() As Integer
	Declare Function isFull() As Integer
	Declare Constructor()
End Type

Constructor RS_channel()

   timestart = 0
   delay = 0
   wfidx = -1
   frequency = -1
   volume = -1
   isOn = 0
End Constructor


' main "class"
Type RetroSound
	_buff(0 To RS_SAMPLESIZE*RS_SAMPLEPERBUFFER-1) As Short
	_channel(RS_MINCHANNEL To RS_MAXCHANNEL) As RS_channel
	_noteBuffer(RS_MINCHANNEL To RS_MAXCHANNEL) As RS_NoteBuffer
#If SOUNDLIB=FBSOUND
	_hSoundTable(RS_MINCHANNEL To RS_MAXCHANNEL,RS_MINWAVEFORM To RS_MAXWAVEFORM) As Integer
#EndIf
	_
	_defaultChannel As Integer
	_defaultWaveForm  As integer
	_defaultVolume As integer
	_note(0 To 127) As integer
	' Intialize the Engine
	Declare Constructor()
	' close the engine
	Declare Destructor()
	' set a waveform into the catalog
	' wfi is the catalog index betwwen RS_MINWAVEFORM and RS_MAXWAVEFORM
	'wf : can be "SINE" "SAW" "SQUARE" "NOISE" or "0123456789ABCDEF" 16 hexadecimal value to define the 16 sample value of the wave form
	Declare Sub setWaveForm(wfi As Integer,ByRef wf As String)
	' heart of the retrosound system
	' channel : output channel between RS_MINCHANNEL and RS_MAXCHANNEL
	' wf : index inside the waveform catalog
	' freq : frequency (30 to 20000)
	' volume : 0 silence, 255 full volume
	' delay : delay in millisecond , 0= never stop, -1 = don't touch current delay (if change volume,frequency or other on the channel) 
	Declare Sub keyOnOff(channel As Integer, wf As Integer, freq As Integer, volume As Integer,  delay As double)
	' same as keyOnOff but delay = 0
	Declare Sub keyOn(channel As Integer,wf As Integer,freq As Integer,volume As Integer)
	' stop playing on channel 
	Declare Sub keyOff(channel As Integer)
	' set default channel to use when use QB sound instruction
	Declare Sub setDefault(channel As Integer, wfidx As Integer, volume As Integer) 
	' play on default channel, at frequency during "delay" millisecond, it's a blocking instruction (wait until end of delay)
	Declare Sub sound(freq As integer, delay As Double) 
	' same as above but can can choice if the call is blocking(_wait=1) or non blocking (_wait=0) or (wait=2) buffered
	Declare Sub sound(freq As integer, delay As Double, _wait As integer) 
	' method that manage the engine, call it in the game loop, or other long loop... otherwise sound delay do not works
	Declare Sub addNote(wfidx As Integer, freq As Integer, vol As integer, delay As Double)
	Declare Sub tick()
End Type

Enum seq_status
	SS_STOP = 0
	SS_PLAY
	SS_PAUSE
	SS_END
End Enum
	ReDim Shared seq(0) As String

Type SequencerCTX
	startindex As integer
	firstchannel As Integer	
	index As Integer
	repeat_addr(0 To 100) As Integer
	repeat_count(0 To 100) As Integer
	repeat_idx As Integer
	sub_stack(0 To 100) As Integer 
	sub_idx As integer
	status As Integer
	Declare Constructor()
	Declare Sub Play()
	Declare Sub Stop()
	Declare Sub Pause()
	Declare Sub Resume()
End Type


Type Sequencer
	ctx(RS_MINCHANNEL To RS_MAXCHANNEL) As SequencerCTX
	tempo As Double
	slicetimer As Double
	rs As RetroSound ptr 
	sub_addr(0 To 100) As Integer
	Declare Constructor()
	Declare Sub slice(cidx As integer)
	Declare Sub tick()
	Declare Sub load(ByRef _rs As RetroSound,seq() As String)
	Declare Sub Play()
	Declare Sub Stop()
	Declare Sub Pause()
	Declare Sub Resume()
		
End Type

' ---------------------
' ----- IMPLEMENTATION


Sub RS_NoteBuffer.addNote(wfidx As Integer, freq As Integer, vol As integer, delay As Double)
	If Not isFull() then
		_noteBuffer(_noteBufferEnd).waveform = wfidx
		_noteBuffer(_noteBufferEnd).frequency = freq
		_noteBuffer(_noteBufferEnd).volume = vol
		_noteBuffer(_noteBufferEnd).delay = delay
		_noteBufferEnd +=1
		If _noteBufferEnd > RS_MAXNOTEBUFFER Then
			_noteBufferEnd = RS_MINNOTEBUFFER
		EndIf
	End If
End Sub

function RS_NoteBuffer.isFull() As integer
	If _noteBufferStart-1 = _noteBufferEnd Or _
	(_noteBufferStart = RS_MINNOTEBUFFER And _noteBufferEnd = RS_MAXNOTEBUFFER) Then
	  Return -1
	End If
	Return 0
End function

Function RS_NoteBuffer.getNote() As RS_Note
	Dim i as integer
	If hasNote() Then
	  i = _noteBufferStart
	  _noteBufferStart+=1
	  If _noteBufferStart > RS_MAXNOTEBUFFER Then
	  	 _noteBufferStart = RS_MINNOTEBUFFER
	  EndIf
	  Return _noteBuffer(i)
	EndIf
	Return _noteBuffer(_noteBufferStart)
End Function
Function RS_NoteBuffer.hasNote() As Integer
	If _noteBufferStart = _noteBufferEnd Then Return 0
	Return -1
End Function
Constructor RS_NoteBuffer()
  _noteBufferStart = RS_MINNOTEBUFFER
  _noteBufferEnd = _noteBufferStart
End Constructor




Constructor SequencerCTX()
	index = 0
	startindex = 0
	status = SS_STOP
	firstchannel = 0
	repeat_idx = -1
	sub_idx = -1
End Constructor

Sub SequencerCTX.Play()
	If status = SS_STOP Or status = SS_END then
		repeat_idx = -1
		sub_idx = -1
		status = SS_PLAY
		index = startindex
	End if
End Sub

Sub SequencerCTX.Pause()
	If status = SS_PLAY then
		status = SS_PAUSE
	End If
End Sub

Sub SequencerCTX.Resume()
	If status = SS_PAUSE Then 
		status = SS_PLAY
	End If
		
End Sub


Sub SequencerCTX.Stop()
	status = SS_STOP
End Sub



Sub Sequencer.slice(cidx As integer)
	Dim sl As String
	Dim cmd As String
	Dim st As String
	Dim goback As Integer
	Dim i As Integer
	Dim c As Integer
	Dim vc As string
	Dim v As Integer
	Dim v2 As integer
	If ctx(cidx).index>UBound(seq) Then
		ctx(cidx).status = SS_END
		Exit sub
	EndIf
	Print cidx;"-";ctx(cidx).index, " ";
	Do
		
		Print seq(ctx(cidx).index);",";
		sl = seq(ctx(cidx).index)
		ctx(cidx).index += 1
		cmd = UCase(Left(sl,1))
		st = cmd
		cmd = Right(sl,Len(sl)-1)
		goback = 0 ' by default stay in loop
		Select Case st
			Case "X" ' Parallel sequencer
				st = UCase(Left(cmd,1))
				cmd = Right(cmd,Len(cmd)-1)
				Select Case st
					Case "I"
						v = Val("&H0"+Left(cmd,1))
						cmd = Right(cmd,Len(cmd)-1)
						v2 = Val(cmd)
						ctx(v).startindex = sub_addr(v2)
					Case "P"
						v = Val("&H0"+Left(cmd,1))
						ctx(v).Play()						
					Case "S"
						v = Val("&H0"+Left(cmd,1))
						ctx(v).Stop()						
				End Select
			Case "W" ' Waveform choice
				c = ctx(cidx).firstchannel
				For i= 1 To Len(cmd)-1 Step 2
					vc = Mid(cmd,i,2)
					If vc <> "__" then
						v = Int(Val("&h0"+vc))
						rs->keyOn(c,v,-1,-1)
					End If
					c+=1
				Next
			Case "V" ' Volume setting
				c = ctx(cidx).firstchannel
				For i= 1 To Len(cmd)-1 Step 2
					vc = Mid(cmd,i,2)
					If vc <> "__" then
						v = Int(Val("&h0"+vc))
						rs->keyOn(c,-1,-1,v)
					End If
					c+=1
				Next
			Case "-" ' Volume setting
				c = ctx(cidx).firstchannel
				For i= 1 To Len(cmd)-1 Step 2
					vc = Mid(cmd,i,2)
					If vc <> "__" then
						v = Int(Val("&h0"+vc))
						v = rs->_channel(c).volume - v
						If v < 0 Then v = 0
						rs->keyOn(c,-1,-1,v)
					End If
					c+=1
				Next
			Case "+" ' Volume setting
				c = ctx(cidx).firstchannel
				For i= 1 To Len(cmd)-1 Step 2
					vc = Mid(cmd,i,2)
					If vc <> "__" then
						v = Int(Val("&h0"+vc))
						v = rs->_channel(c).volume + v
						If v > 255 Then v = 255
						rs->keyOn(c,-1,-1,v)
					End If
					c+=1
				Next
			Case "N"	' Note Playing
				c = ctx(cidx).firstchannel
				For i= 1 To Len(cmd)-1 Step 2
					vc = Mid(cmd,i,2)
					If vc <> "__" then
						v = Int(Val("&h0"+vc))
						rs->keyOn(c,-1,rs->_note(v),-1)
					End If
					c+=1
				Next
				'goback = 1
			Case "@" ' beginning of repeating sequence
				ctx(cidx).repeat_idx +=1
				ctx(cidx).repeat_addr(ctx(cidx).repeat_idx)= ctx(cidx).index
				ctx(cidx).repeat_count(ctx(cidx).repeat_idx) = Int(Val(cmd))-1			
			Case "L" ' sequence looping of the last valid repeat sequence
				If ctx(cidx).repeat_idx>=0 Then
					If ctx(cidx).repeat_count(ctx(cidx).repeat_idx)<>0 Then
						ctx(cidx).repeat_count(ctx(cidx).repeat_idx)-=1
						ctx(cidx).index = ctx(cidx).repeat_addr(ctx(cidx).repeat_idx)
					Else
						If ctx(cidx).repeat_count(ctx(cidx).repeat_idx)=0 Then
							ctx(cidx).repeat_idx -=1
						EndIf
					EndIf
				EndIf
			Case "T" ' change tempo
				tempo = Int(Val(cmd))*1.0/1000.0
'			Case "#" ' Marker for the beginning a sub sequence
'				v = Int(Val(cmd))
'				sub_addr(v)= index
			Case "R" ' return for a sub sequence
				If ctx(cidx).sub_idx >=0 Then
					ctx(cidx).index = ctx(cidx).sub_stack(ctx(cidx).sub_idx)
					ctx(cidx).sub_idx-=1
				Else
					ctx(cidx).status = SS_END
					goback = 1						
				EndIf
			Case "G" ' Go to a sub sequence
				v = Int(Val(cmd))
				ctx(cidx).sub_idx+=1
				ctx(cidx).sub_stack(ctx(cidx).sub_idx) = ctx(cidx).index
				ctx(cidx).index = sub_addr(v)
			Case "_" ' end of position
				goback = 1
			Case "$" ' end of song
				ctx(cidx).status = SS_END
				goback = 1
		End Select
		If ctx(cidx).index>UBound(seq) Then
			ctx(cidx).status = SS_END
			Exit sub
		EndIf
	Loop Until goback=1
	Print ""
End Sub

Constructor Sequencer()
   Dim i As Integer
   Dim j As integer
	tempo=100.0/1000.0
	j = 0
	For i = LBound(ctx) To UBound(ctx)
		ctx(i).firstchannel = j
		j += 1 
	Next
End Constructor

Sub Sequencer.tick()
	Dim i As Integer
	If Timer - (slicetimer+tempo) >= 0 Then
		slicetimer = Timer
		For i = LBound(ctx) To UBound(ctx)
			If ctx(i).status = SS_PLAY And ctx(0).status = SS_PLAY Then
				slice(i)
			Else
				If i = 0 Then Exit for
			End If
		Next i
	End If
	rs->tick()
End Sub


Sub Sequencer.Load(ByRef _rs As RetroSound, _seq() As String)
	Dim i As Integer
	Dim cmd As String
	ReDim  seq (LBound(_seq) To UBound(_seq)) As String
	For i = LBound(seq) To UBound(seq)
		seq(i) = _seq(i)	
		cmd = seq(i)
		If Left(cmd,1)="#" Then
			cmd = Right(cmd,Len(cmd)-1)
			sub_addr(Int(Val(cmd)))=i+1
		EndIf
	Next
	ctx(0).startindex = LBound(seq)
	rs = @_rs
End Sub

Sub Sequencer.Play()
	ctx(0).Play
End Sub

Sub Sequencer.Pause()
	ctx(0).Pause()
End Sub

Sub Sequencer.Resume()
	ctx(0).Resume()
End Sub


Sub Sequencer.Stop()
	Dim i As Integer
	For i = LBound(ctx) To UBound(ctx)
		ctx(i).stop()
		rs->Keyoff(i)
	Next i
	
End Sub

Constructor RetroSound()


	Dim channel1 As Integer
	Dim  As Integer i,ii
	


#If SOUNDLIB=FBSOUND
	fbs_Init(44100,1)
#EndIf

	'**** Create NOISE sample
	Dim bb(0 To 9999) As Short
	For i = 0 To 9999
		bb(i)= Int((Rnd*1.0-0.5)*32767)
	Next

#If SOUNDLIB=FBSOUND
	Dim As integer hWave,hSound
	Dim As FBS_SAMPLE Ptr  lpSamples
	For i = RS_MINCHANNEL To RS_MAXCHANNEL
		fbs_create_Wave(10000,@hWave,@lpSamples)
		For ii = 0 To 10000-1
			lpSamples[ii] = bb(ii)
		Next
		fbs_create_Sound(hWave,@hSound)
		_hSoundTable(i,RS_MAXWAVEFORM) = hSound
		fbs_Set_SoundVolume(hSound,0)
	Next i
#endif	

	' initialize default waveform catalog
	setWaveForm(RS_MINWAVEFORM+0,"SINE") 
	setWaveForm(RS_MINWAVEFORM+1,"SAW") 
	setWaveForm(RS_MINWAVEFORM+2,"SQUARE")
	setWaveForm(RS_MINWAVEFORM+3,"0F1E2D3C4B5A6978") ' CUSTOM special
	setWaveForm(RS_MINWAVEFORM+4,"02468ACEECA86420") ' ledder Up and down
	setDefault(RS_MINCHANNEL,RS_MINWAVEFORM+2,128)
	For i = 5 To RS_MAXWAVEFORM-1
		setWaveForm(RS_MINWAVEFORM+i,"NOISE")
	Next

	' initialize note
	For i = 0 To 127
		_note(i) = Int(440.0*(2^((i-57)/12))+0.5)
		If _note(i) < 30 Then _note(i) = 30
		If _note(i) > 10000 Then _note(i) = 10000
	Next

End Constructor


Destructor RetroSound()


#If SOUNDLIB=FBSOUND
	fbs_Exit()
#EndIf


End Destructor

Sub RetroSound.setWaveForm(wfi As Integer,ByRef wf As String)
	' Create sample (in memory , with a 16bits signed monophonic structure)
	Dim As Integer i,ii
#If SOUNDLIB=FBSOUND	
	Dim As integer hWave,hSound
	Dim As FBS_SAMPLE Ptr  lpSamples
#EndIf

	If Len(wf) >=16 Then ' custom wave
		'ii = 1
		For i = LBound(_buff) To  RS_SAMPLESIZE-1 'UBound(_buff)
			ii = Int((i*1.0/((RS_SAMPLESIZE*1.0)/(Len(wf)*1.0)))+1) 
			_buff(i) = Int((Val("&h0"+Mid(wf,ii,1))/15.0-0.5) *32767) 
			'ii = ii +1
		Next
	End if		
	If ucase(wf) = "SINE" Then
		For i = LBound(_buff) To UBound(_buff)
			_buff(i) = Int(Sin(2*PI/RS_SAMPLESIZE*i)*32767)
		Next
	End if		
	If ucase(wf) = "SAW" Then
		For i = LBound(_buff) To UBound(_buff)
			_buff(i) = (i*1.0/RS_SAMPLESIZE-0.5)*32767
		Next
	End if		
	If ucase(wf) = "SQUARE" Then
		For i = LBound(_buff) To UBound(_buff)
			_buff(i) = IIf(i<RS_SAMPLESIZE/2,32767,-32767)
		Next
	End if		
	If ucase(wf) = "NOISE" Then
		For i = LBound(_buff) To UBound(_buff)
			_buff(i) = Int((Rnd*1.0-0.5)*32767.0)
		Next
	End if		
#If SOUNDLIB=FBSOUND
	For i = RS_MINCHANNEL To RS_MAXCHANNEL
		fbs_create_Wave(RS_SAMPLESIZE*RS_SAMPLEPERBUFFER,@hWave,@lpSamples)
		For ii = 0 To RS_SAMPLESIZE*RS_SAMPLEPERBUFFER-1
			lpSamples[ii] = _buff(ii Mod RS_SAMPLESIZE)
		Next
		fbs_create_Sound(hWave,@hSound)
		_hSoundTable(i,wfi) = hSound
		fbs_Set_SoundVolume(hSound,0)
	Next i
#endif
	
End Sub

Sub RetroSound.keyOnOff(channel As Integer,wf As Integer,freq As Integer,volume As Integer, delay As double)
	'Print "Channel:";channel,"waveform:";wf,"frequence:";freq,"volume:";volume,"delay:";delay
	If freq = 0 Then
		 KeyOff(channel)
		 Exit sub
	EndIf
	If _channel(channel).wfidx <> wf And wf >=0 Then


#If SOUNDLIB=FBSOUND
		If _channel(channel).wfidx >=RS_MINWAVEFORM then
			fbs_Set_SoundVolume(_hSoundTable(channel,_channel(channel).wfidx),0)
		End If
		fbs_Play_Sound(_hSoundTable(channel,wf),&h0fffffff)
#EndIf	
		_channel(channel).wfidx = wf
		_channel(channel).frequency = -1
		_channel(channel).volume = -1
	End If
	If _channel(channel).frequency <> freq And freq >=0 Then

#If SOUNDLIB=FBSOUND
		If wf = -1 Then
			If _channel(channel).wfidx = RS_MAXWAVEFORM then
				fbs_Set_SoundSpeed(_hSoundTable(channel,_channel(channel).wfidx),freq/(44100.0/16.0))
			Else
				fbs_Set_SoundSpeed(_hSoundTable(channel,_channel(channel).wfidx),freq/(44100.0/RS_SAMPLESIZE))
			endif				
		Else	
			If wf = RS_MAXWAVEFORM then
				fbs_Set_SoundSpeed(_hSoundTable(channel,wf),freq/(44100.0/16.0))
			Else
				fbs_Set_SoundSpeed(_hSoundTable(channel,wf),freq/(44100.0/RS_SAMPLESIZE))
			endif				
		end If	
#EndIf
		_channel(channel).frequency = freq
	End If
	If _channel(channel).volume <> volume And volume>=0 Then

#If SOUNDLIB=FBSOUND
		If wf= -1 then
			fbs_Set_SoundVolume(_hSoundTable(channel,_channel(channel).wfidx),volume/255.0)
		Else
			fbs_Set_SoundVolume(_hSoundTable(channel,wf),volume/255.0)
		End If
		
#endif	
		_channel(channel).volume = volume
	EndIf
	If delay >=0 Then 
		_channel(channel).delay = delay/1000.0
		If delay>0 Then _channel(channel).timestart = Timer
	End if	
	_channel(channel).isOn = -1
End Sub

Sub RetroSound.keyOn(channel As Integer,wf As Integer,freq As Integer,volume As Integer)
	Dim As Double delay
	delay = 0
	keyOnOff(channel, wf, freq, volume, delay)
End Sub


Sub RetroSound.keyOff(channel As Integer)
	If _channel(channel).isOn Then
		keyOn(channel,_channel(channel).wfidx,_channel(channel).frequency,0)		
	EndIf
	_channel(channel).isOn = 0
End Sub


Sub RetroSound.setDefault(channel As Integer,wfidx As Integer,volume As integer)
	_defaultChannel = channel 
	_defaultWaveForm = wfidx
	_defaultVolume = volume
	keyOnOff(_defaultchannel,_defaultWaveForm,440,0,0)
End Sub

Sub RetroSound.sound(freq As integer, delay As Double)
	sound(freq,delay,1)
End Sub

Sub RetroSound.sound(freq As integer, delay As Double, _wait As integer )
	Dim As Integer f
	Dim As Double d
	f = freq
	If f <=0 And f >=-127 Then f = _note(-f)
	If f <0 Then f= -f
	d = delay
	If _wait = 0 Or _wait = 1 then
		keyOnOff(_defaultChannel,_channel(_defaultChannel).wfidx,f,_defaultVolume,d)
		If _wait And delay > 0 Then
			While _channel( _defaultChannel).isOn
				Sleep 1,1
				tick()
			Wend
		EndIf
	End If
	If _wait = 2 And d > 0 Then
		Do
			Sleep 1,1
			tick()
		Loop Until Not _noteBuffer(_defaultChannel).isFull()
		_noteBuffer(_defaultChannel).addNote(_channel(_defaultChannel).wfidx,f,_defaultVolume,d)
	EndIf
	
End Sub 

Sub RetroSound.tick()
	Dim As Integer i
	Dim As Double tm,delta1,delta2
	Dim noteoff As double
	For i = RS_MINCHANNEL To RS_MAXCHANNEL
		If _channel(i).delay > 0 Then
			tm = Timer
			noteoff = Int((_channel(i).delay*1000.0- Int(_channel(i).delay*1000.0+0.5))*1000.0)/1000.0
	'		Print _channel(i).delay*1000,noteoff*1000
			'noteoff = 0
			delta1 = tm-(_channel(i).timestart+_channel(i).delay-noteoff)
			delta2 = tm-(_channel(i).timestart+_channel(i).delay)
			If delta1>=0 Then
				keyOnOff(i,_channel(i).wfidx,_channel(i).frequency,0,-1)
			EndIf
			If delta2>=0 Then
				KeyOff(i)
				_channel(i).delay = 0
			EndIf
		End If
		If _channel(i).delay = 0 Then		
			If _noteBuffer(i).hasNote() Then
				Dim aNote As RS_Note
				aNote = _noteBuffer(i).getNote()
				keyOnOff(i,aNote.waveform,aNote.frequency,aNote.volume,aNote.delay)
			EndIf
		EndIf
	Next

End Sub

#Include "fbgfx.bi"
'#Define PI 3.141592654
#Define G_WIDTH 640
#Define G_HEIGHT 480
#Define KW (G_WIDTH/320)
#Define KH (G_HEIGHT/240)
#Define CHEAT 1
'**********************
'   Declaration types
'**********************

Type TV_Polar2D
	radius As Double
	angle As double
End Type

Type TV_Vertex2D
	x As Double
	y As Double
End Type

Type TinyVectrex
	boom As Double
	plot(0 To 17) As TV_Polar2D
	plotxy(0 To 17) As TV_Vertex2D
	size As Double
	angle As Double
	cx As Double
	alphabet(0 To 255) As String
	Declare Constructor()
	Declare Sub SetCenterText(cenx As Double) ' between 0.0 (extreme left of text) and 1.0 (extreme right of text), default 0.5 (middle)
	Declare Sub scaleRot(psize As Double,angle As Double)
	Declare Sub DrawScript(ByRef s As String, xc As Integer,yc As Integer,colour As integer)
	Declare Sub DrawScript(ByRef s As String, xc As Integer,yc As Integer,colour As Integer,explode As Double)
	Declare Sub DrawText(ByRef s As String, xc As Integer,yc As Integer,colour As Integer)
	Declare Sub DrawText(ByRef s As String, xc As Integer,yc As Integer,colour As Integer, txtAngle As double)
End Type


Enum game_status
	GS_START=0
	GS_INTRO
	GS_RUN
	GS_PAUSE
	GS_CRASHED
	GS_LANDED
	GS_GAMEOVER
	GS_FINISH
	GS_EDIT
	GS_EDIT_TEXT
End Enum

Enum player_action
   PA_NOTHING = 0
   PA_LEFT
   PA_RIGHT
   PA_THRUST
   PA_QUIT
   PA_PAUSE
End Enum

Enum lander_status
	LS_NORMAL = 0
	LS_THRUST
	LS_LANDED
	LS_LANDED_NOSKY
	LS_CRASH
End Enum

Enum EnergySucker_Status
	SS_NORMAL = 0
	SS_EXPLODED
End Enum

Type Vertex2D
	x As Double
	y As Double
End Type

Type Lander
	model As TinyVectrex
	location As Vertex2D
	Speed As vertex2D
	fuel As Double
	angle As Double
	size As Double
	status As lander_status
	crash_tic As Integer
	tic As integer
	Declare Constructor()
	Declare Constructor(x As Double, y As double)
	Declare Sub init()
	Declare Sub Draw()
End Type

Type EnergySucker
	model As TinyVectrex
	location As Vertex2D
	Speed As Vertex2D
	angle As Double
	size As Double
	tic As Integer
	status As EnergySucker_status
	Declare Constructor
	Declare Sub Draw()
	Declare Sub init()
	Declare Sub init (x As Double, y As Double)
End Type

Type Landscape
	ground(0 To 319) As Double
	sky(0 To 319) As Double
	padLocation As Vertex2D
	fuelLocation As Vertex2D
	gravity As Double
	startlocation As Vertex2D
	inverse As Integer
	go_up As Integer
	go_down As Integer
	go_left As Integer
	go_right As Integer
	allowtakeoff As Integer
	landedmsg As string
	Declare Constructor()
	Declare Sub Draw()
	Declare Sub init()
	Declare Sub init(ByVal Glevel As Integer)
End Type

Type game
	
	tic As integer
	tic2 As integer
	life As Integer
	bestsafeland As Integer
	safeland As Integer
	sublevelx As Integer
	sublevely As integer
	bestscore As integer
	score As integer
	ship As lander
	Sucker(0 To 100) As EnergySucker
	nbSucker As integer
	scene As landscape
	playerAct As player_action
	status As game_status
	msg(0 To 200) As String
	xm As double
	ym As double
	bm As integer
	Declare Constructor ()
	Declare Destructor()
	Declare Sub init()
	Declare Sub initgfx()
	Declare Sub Draw()
	Declare Sub initLevel(level As integer)
	Declare Sub EnergySuckerAction()
	Declare Sub MainLoop()
	Declare function LoadLevel(lvl As Integer) As Integer
	Declare function SaveLevel(lvl As Integer) As Integer
	Declare function LoadLevel(lvl As Integer,sublvlx As Integer,sublvly As integer) As Integer
	Declare function SaveLevel(lvl As Integer,sublvlx As Integer,sublvly As Integer) As Integer
	Declare Sub showmessage()
End Type

'**********************
'       Globals
'**********************
 
Dim Shared text As TinyVectrex
Dim Shared boardtxt As TinyVectrex 
Dim Shared text2 As TinyVectrex 
Dim Shared smalltxt As TinyVectrex 
Dim Shared bigtxt As TinyVectrex 
Dim Shared rs As RetroSound

smalltxt.scaleRot(2*KW,0)
bigtxt.scaleRot(5*KW,0)
text.scaleRot(4*KW,0)
text2.scaleRot(4*KW,0)
boardtxt.scaleRot(3.5*KW,0)
boardtxt.SetCenterText(0)
'**********************
'    Implementation
'**********************

Sub TinyVectrex.SetCenterText(cenx As Double)
	this.cx = cenx
End Sub
 
Sub TinyVectrex.scaleRot(psize As Double,pangle As Double)
	Dim i As Integer
	For i = 0 To 17
		plotxy(i).x = psize*Cos(pangle+plot(i).angle)*plot(i).radius
		plotxy(i).y = psize*Sin(pangle+plot(i).angle)*plot(i).radius
	Next
	this.size = psize
	this.angle = pangle
End Sub

Sub TinyVectrex.DrawScript(ByRef s As String, xc As Integer,yc As Integer, colour As Integer)
   this.DrawScript(s, xc,yc, colour, this.boom)
End Sub 

Sub TinyVectrex.DrawScript(ByRef s As String, xc As Integer,yc As Integer, colour As Integer, explode As double)
	Dim i As Integer
	Dim ss As String
	Dim c As String
	Dim b As integer
	Dim a As Integer
	Dim As Double x,y,xo,yo,x1,y1,x2,y2,xx,yy,k
	
	
	k = 1 : b=0
	ss = UCase(" "+s)
	Dim idx As integer
	For i = 1 To Len(ss)
		c = Mid(ss,i,1)
		If c= " " Then
			b = 1
		Else
			a = Asc(c)
			If a>=65 And a<(65+17) Then
				a-=65
				If k = 1 Then 
					x = plotxy(a).x+xc
					y = plotxy(a).y+yc
				Else
					x = plotxy(a).x*k+xc
					y = plotxy(a).y*k+yc
				End If
				If b = 0 Then 
					If explode = 1 Then 
						Line(xo,yo)-(x,y),colour
					Else
						xx = (xo+x)/2-xc     : yy = (yo+y)/2 - yc
						xx = xx*explode - xx : yy = yy*explode - yy
						x1 = xo + xx         : y1 = yo + yy 
						x2 = x + xx          : y2 = y + yy 
						Line(x1,y1)-(x2,y2),colour
					End If
				End If
				xo = x : yo = y
				b = 0
			Else
				If a >=48 And a <=57 Then
					Select Case a
						Case 48 
							k = 1
						Case 49
							k = 0.70710678118654752440084436210485
						Case Else
							k = 0.70710678118654752440084436210485^(a-48)				
					End Select
				EndIf
			EndIf
		EndIf
	Next i
End Sub

Sub TinyVectrex.DrawText(ByRef s As String, xc As Integer,yc As Integer,colour As Integer)
	this.DrawText(s,xc,yc,colour,this.angle)
End Sub


Sub TinyVectrex.DrawText(ByRef s As String, xc As Integer,yc As Integer,colour As Integer, txtAngle As double)
	Dim i As Integer
	Dim a As Integer
	Dim c As integer
	Dim As Double dx,dy
	c = Int(Len(s)*this.cx)
	dx = this.size*Cos(txtAngle)*1.80
	dy = this.size*Sin(txtAngle)*1.80
	For i = 1 To Len(s)
		a = Asc(Mid(s,i,1))
		this.DrawScript(alphabet(a),xc+dx*(i-1-c),yc+dy*(i-1-c),colour)
	Next
End Sub



Constructor TinyVectrex()
	Dim i As Double
	Dim j As Integer
	Dim k As Integer
	dim As Integer x,y
	boom = 1
	SetCenterText(0.5)
	j = 0
	this.plot(j)= Type<TV_Polar2D>(0,0)
	j+=1
	For i = 0 To 2* PI-PI/4 Step PI/4
		this.plot(j) = Type<TV_Polar2D>(1,i)
		this.plot(j+8) = Type<TV_Polar2D>(Sqr(2)/2,i)
		j + = 1
	Next
	scaleRot(4,0)
	' sign
	this.alphabet(Asc("+")) = "PL NJ" :	this.alphabet(Asc("-")) = "NJ" :	this.alphabet(Asc("*")) = "OK MQ NJ"
	this.alphabet(Asc("/")) = "IE" :	this.alphabet(Asc(":")) = "PP LL" :	this.alphabet(Asc("!")) = "PA LL"
	this.alphabet(Asc(".")) = "LL" :	this.alphabet(Asc(",")) = "LD"
	this.alphabet(Asc("'")) = "HP" :	this.alphabet(Asc("(")) = "HGED" :	this.alphabet(Asc(")")) = "HICD"
	this.alphabet(Asc("_")) = "EC"
	this.alphabet(asc("~")) = "2E0D2C 2HCBDFEH"  'small ship with thrust
	this.alphabet(asc("^")) = "2HCBDFEH" ' small ship without thrust
	this.alphabet(asc("$")) = "HCBDFEH" ' normal sized ship
	this.alphabet(asc("&")) = "2HCBDFEH 0BCDEFGHIB" ' rounded lander
	this.alphabet(Asc("#")) = "JKLMNOPQJ BJ CK DL EM FN GO HP IQ" ' sun
	this.alphabet(Asc("~")) = "GICEG BDFHB" ' star
	this.alphabet(Asc("§")) = "0H3I0B3C0D3E0F3G0H 3I0I 3C0C 3E0E 3G0G" ' Energy sucker
	this.alphabet(Asc("%")) = "BHFNECJB" ' Arrow

	' Digit
	this.alphabet(asc("0")) = "EIHGEDCI" 
	this.alphabet(asc("1")) = "GHL"
	this.alphabet(asc("2")) = "GHIEC"
	this.alphabet(asc("3")) = "GHIACDE"
	this.alphabet(asc("4")) = "LHNJ"
	this.alphabet(asc("5")) = "IGNJCDE"
	this.alphabet(asc("6")) = "IHGEDCJN"
	this.alphabet(asc("7")) = "GIL"
	this.alphabet(Asc("8")) = "NGHICDENJ"
	this.alphabet(asc("9")) = "JNGHICDE"
	' Alphabet Upercase
	this.alphabet(asc("A")) = "ENOPQJC NJ"
	this.alphabet(asc("B")) = "EGPQAKLE NA"
	this.alphabet(asc("C")) = "KLMNOPQ"
	this.alphabet(asc("D")) = "EGPQJKLE"
	this.alphabet(asc("E")) = "IGEC NA"
	this.alphabet(asc("F")) = "IGE NA"
	this.alphabet(asc("G")) = "AJKLMNOPQ"
	this.alphabet(asc("H")) = "GE IC NJ"
	this.alphabet(asc("I")) = "PL"
	this.alphabet(asc("J")) = "QKLM"
	this.alphabet(asc("K")) = "GE NP NC"
	this.alphabet(asc("L")) = "GEC"
	this.alphabet(asc("M")) = "EGAIC"
	this.alphabet(asc("N")) = "EGCI"
	this.alphabet(asc("O")) = "JKLMNOPQJ"
	this.alphabet(asc("P")) = "EGPQAN"
	this.alphabet(asc("Q")) = "AKLMNOPQJK"
	this.alphabet(asc("R")) = "EGPQAN AC"
	this.alphabet(asc("S")) = "QPOKLM"
	this.alphabet(asc("T")) = "PL GI"
	this.alphabet(asc("U")) = "GNMLKJI"
	this.alphabet(asc("V")) = "GLI"
	this.alphabet(asc("W")) = "GEACI"
	this.alphabet(asc("X")) = "GC EI"
	this.alphabet(asc("Y")) = "GAI AL"
	this.alphabet(asc("Z")) = "GIEC"
	'Alphabet lower case
	this.alphabet(asc("a")) = "1ENOPQJC NJ"
	this.alphabet(asc("b")) = "1EGPQAKLE NA"
	this.alphabet(asc("c")) = "1KLMNOPQ"
	this.alphabet(asc("d")) = "1EGPQJKLE"
	this.alphabet(asc("e")) = "1IGEC NA"
	this.alphabet(asc("f")) = "1IGE NA"
	this.alphabet(asc("g")) = "1AJKLMNOPQ"
	this.alphabet(asc("h")) = "1GE IC NJ"
	this.alphabet(asc("i")) = "1PL"
	this.alphabet(asc("j")) = "1QKLM"
	this.alphabet(asc("k")) = "1GE NP NC"
	this.alphabet(asc("l")) = "1GEC"
	this.alphabet(asc("m")) = "1EGAIC"
	this.alphabet(asc("n")) = "1EGCI"
	this.alphabet(asc("o")) = "1JKLMNOPQJ"
	this.alphabet(asc("p")) = "1EGPQAN"
	this.alphabet(asc("q")) = "1AKLMNOPQJK"
	this.alphabet(asc("r")) = "1EGPQAN AC"
	this.alphabet(asc("s")) = "1QPOKLM"
	this.alphabet(asc("t")) = "1PL GI"
	this.alphabet(asc("u")) = "1GNMLKJI"
	this.alphabet(asc("v")) = "1GLI"
	this.alphabet(asc("w")) = "1GEACI"
	this.alphabet(asc("x")) = "1GC EI"
	this.alphabet(asc("y")) = "1GAI AL"
	this.alphabet(asc("z")) = "1GIEC"

End Constructor

#Define LANDER_NORMAL "2HCBDFEH"
#Define LANDER_LANDED "2HCBDFEH"
#Define LANDER_THRUST "2E0D2C 2HCBDFEH"

Sub lander.init()
	this.location.x = 160
	this.location.y = 230
	this.fuel = 100
	this.angle = 0
	this.size = 7
	this.status = LS_NORMAL
	this.crash_tic = 0
	this.tic = 0
	this.model.boom = 1
End Sub

Constructor lander()
	this.init
	'rs.keyOn(1,15,50,0)
End Constructor

Constructor lander(x As Double , y As Double)
   this.init
	this.location.x = x
	this.location.y = y
End Constructor

Sub lander.Draw()
'	Static tic As Integer
	Dim As Integer v1,v2
	this.model.scaleRot(this.size*2*KW,this.angle)
	this.tic+=1
	Select Case this.status
		Case LS_NORMAL
			crash_tic = 0
			this.model.boom = 1
			this.model.DrawScript(LANDER_NORMAL,this.location.x*KW,(240-this.location.y)*KH,10)
			If this.fuel <= 0 then
		   	smalltxt.DrawText("I'M CRASHING!",this.location.x*KW,(240-this.location.y-this.size-7)*KH,10)
			End If

		Case LS_LANDED, LS_LANDED_NOSKY
			v1 = rs._channel(1).volume
			v1 = v1 - 1
			v2 = rs._channel(1).frequency
			v2 = v2-1
			If v2 < 30 Then v2 = 30
			If v1 < 0 Then v1 = 0
			rs.keyonoff(1,15,v2,v1,100)
			crash_tic = 0
			this.model.boom = 1
			this.model.DrawScript(LANDER_NORMAL,this.location.x*KW,(240-this.location.y)*KH,10)
			Circle(this.location.x*KW,(240-this.location.y)*KH),(this.size+5+3*Sin(this.tic/10))*KW,10
			If this.fuel >=70  And this.fuel <=99 Then 
				If this.status = LS_LANDED then
					smalltxt.DrawText("Launch To "+Str(3-Int((this.fuel-70.0)/30*4)),this.location.x*KW,(240-this.location.y-this.size-7)*KH,10)
				Else
					smalltxt.DrawText("Ready !",this.location.x*KW,(240-this.location.y-this.size-7)*KH,10)
				End If
			End if
			If this.fuel >=100 And this.location.y < 300 And this.status =LS_LANDED Then
				If this.location.y < 255 Then
					v1 = 128- this.location.y/2
				Else
					v1 = 0
				EndIf 
				rs.keyon(1,15,this.location.y*10,v1)
				this.angle = 0
				this.location.y*=1.01
			EndIf
		Case LS_CRASH	
			crash_tic +=1
			If crash_tic < 60 Then
				rs.keyonoff(1,15,20+crash_tic/10,255-255*crash_tic/60,100)
				rs.keyonoff(2,15,25+crash_tic/10,255-255*crash_tic/60,100)
				rs.keyonoff(3,15,22+crash_tic/10,255-255*crash_tic/60,100)
				this.model.boom = 1 - crash_tic / 15.0
				this.model.DrawScript(LANDER_NORMAL,this.location.x*KW,(240-this.location.y)*KH,10)
			Else
				rs.keyoff(1):rs.keyoff(2): rs.keyoff(3)
			End if
		Case LS_THRUST
			rs.keyonoff(1,15,this.fuel*20,40,100)
			If (tic Mod 20) <  this.fuel/5 And this.fuel>0 Then
				this.model.DrawScript(LANDER_THRUST,this.location.x*KW,(240-this.location.y)*KH,10)
			Else
				this.model.DrawScript(LANDER_NORMAL,this.location.x*KW,(240-this.location.y)*KH,10)
			EndIf
	End Select
End Sub


#Define EnergySucker_NORMAL "0H3I0B3C0D3E0F3G0H 3I0I 3C0C 3E0E 3G0G"

Constructor EnergySucker()
	this.init()
End Constructor

Sub EnergySucker.Draw()
	this.tic +=1
	Select Case this.status
		Case SS_NORMAL
			this.model.scaleRot(this.size*KW,this.tic*PI/90.0)
			this.model.DrawScript(EnergySucker_NORMAL,this.location.x*KW,(240-this.location.y)*KH,10) 
		Case SS_EXPLODED
	End Select
End Sub

Sub EnergySucker.init()
	this.tic = 0
	this.status = SS_NORMAL
	this.location.x = 0
	this.location.y = 0
	this.speed.x = 0
	this.speed.y = 0
	this.angle = 0
	this.size = 7
	this.model.boom = 1
End Sub

Sub EnergySucker.init (x As Double, y As Double)
	this.init()
	this.location.x = x
	this.location.y = y
End Sub


Constructor Landscape
	this.init()
End Constructor

Sub Landscape.Draw()
	Dim As Integer i,lb,ub
	lb = LBound(this.ground)
	ub = UBound(this.ground)
	For i = lb To ub-1
		If this.ground(i) <> this.sky(i) or this.ground(i+1) <> this.sky(i+1)   Then
			PSet(i*KW,(240-Int(this.ground(i)))*KH),11
			If (this.ground(i) >=20) and this.ground(i+1)>=20 then
				Line(i*KW,(240-Int(this.ground(i)))*KH)-((i+1)*KW,(240-Int(this.ground(i+1)))*KH),10
			End if
			Line(i*KW,(240-Int(this.sky(i)))*KH)-((i+1)*KW,(240-Int(this.sky(i+1)))*KH),10
		End if
	Next
	Line(Int(this.padlocation.x-10)*KW,(240-Int(this.padlocation.y))*KH)-(Int(this.padlocation.x+10)*KW,(245-Int(this.padlocation.y))*KH),10,bf
	smalltxt.DrawText("Target",(this.padlocation.x+1)*KW,(240-this.padlocation.y+8)*KH,10)
	Line(Int(this.fuellocation.x-10)*KW,(240-Int(this.fuellocation.y))*KH)-(Int(this.fuellocation.x+10)*KW,(245-Int(this.fuellocation.y))*KH),10,bf
	smalltxt.DrawText("Reload",(this.fuellocation.x+1)*KW,(240-this.fuellocation.y+8)*KH,10)
End Sub


Sub Landscape.init()
	this.init(0)
End Sub

Sub Landscape.init(ByVal Glevel As Integer)
	Dim As Integer i,j,k,lb,cnt,ub,level 
	Dim As Double cur,prev,slope,delta,mini,maxi,ground
	this.gravity = 0.0025*(int(Glevel/10.0)/3.0+1.0)
	inverse = 0
	allowtakeoff = 1
	go_up = 0
	go_down=0
	go_left= 0
	go_right=0
	landedmsg = "LANDED"
	level = Glevel Mod 10
	lb = LBound(this.ground)
	ub = Ubound(this.ground)
	this.startlocation.x = 160
	this.startlocation.y = 230
	prev = Rnd*200+20
	For i = lb To ub
		this.sky(i) = 250
		this.ground(i) = prev
	Next i 
	For j = 1 To 6
		slope = Rnd*(level+1)*250/j-((level+1)*250/2/j)
		cnt = 0
		For i =  lb To ub 
			cur = this.ground(i) + slope*cnt
			this.ground(i) = cur
			cnt += 1
			If cnt > 320/(2^j) Then
				slope = Rnd*(level+1)*250/j-((level+1)*250/2/j)
				cnt = 0
				If i < ub Then
					delta = cur - this.ground(i+1)
				Else
					delta = 0
				EndIf
				For k = i+1 To ub
					this.ground(k)+= delta
				Next
			EndIf
		Next i
	Next j
	delta = 0
	mini = 1000
	maxi = 0
	For i = lb To ub
		mini = IIf(this.ground(i) < mini, this.ground(i),mini)
		maxi = IIf(this.ground(i) > maxi, this.ground(i),maxi)
		delta += this.ground(i)
	Next i
	ground = (maxi-mini)/5*level
	mini = maxi-ground
	For i = lb To ub
		If this.ground(i) < mini Then this.ground(i) = mini
		this.ground(i)= (this.ground(i)-mini)/(maxi-mini)*(level/8.0)*200+20
	Next i
'	delta = (delta / ub-lb+1)/((level+1)*10)
	For i = lb To ub 
' this.ground(i)/=delta
'		this.ground(i)+= 10
		If this.ground(i) < 20 Then this.ground(i)= 20
		If this.ground(i) > 220 Then this.ground(i)= 220
	Next
	
	this.fuellocation.x = -100
	this.fuellocation.y = -100
	this.padlocation.x= (Int(Rnd*2) *300-150)*(level/10.0)+160
	this.padlocation.y = (this.ground(Int(this.padlocation.x)-10) + this.ground(Int(this.padlocation.x)+10))/2
	If this.padlocation.y<20 Then this.padlocation.y = 20
	lb = Int(this.padlocation.x)-10
	ub = Int(this.padlocation.x)+10
	For i = lb To ub
		this.ground(i) = this.padlocation.y 
	Next
	
End Sub


Constructor game()
	Dim i As Integer
	Dim fi As Integer
	fi = FreeFile
	this.initGfx
	this.init
	this.bestsafeland = 0
	this.bestscore = 0
	
	If Open("lander.sco" For Input As #fi)=0 Then
		Input #fi,this.bestscore
		Input #fi,this.bestsafeland
		Close #fi
	EndIf
	For i = LBound(this.msg) To UBound(this.msg)
		this.msg(i) = "Press Any Key to Start"
	Next i
	this.msg(1) = "Another easy one,"
	this.msg(2) = "Looks to be the same"
	this.msg(3) = "Detecting FOE not far away"
	this.msg(4) = "ALERT ! § Energy sucker ! Avoid it !"
	this.msg(5) = "It was easy... ! But it still here !"
	this.msg(6) = "1 more § !"
	this.msg(7) = "Enegy Sucker Engine Enhanced !"
	this.msg(8) = "BEWARE ! They fly a bit faster!"
	this.msg(9) = "§ speed linit ! But they are 3 !"
	this.msg(10) = "New planet with higher gravity"
	this.msg(11) = "Hope you're not tired"
	this.msg(12) = "He he ! 4 suckers now !"
	this.msg(13) = "Again !"
	this.msg(14) = "Again !"
	this.msg(15) = "Again !"
	this.msg(16) = "Oooh ! 5 Energy Suckers Now"
	this.msg(17) = "Again !"
	this.msg(18) = "Again !"
	this.msg(19) = "One more sucker ! "
	this.msg(20) = "New planet with higher gravity and 6 § !"
	this.msg(21) = "Keep going almost finished !"
	this.msg(22) = "Grr ! Here come another one"
	this.msg(23) = "----------"
End Constructor

Sub game.init()
	this.safeland = 0
	this.score = 0
	this.life = 3
	this.status=GS_INTRO
	this.tic = 0	
	this.tic2 = 0	
	this.nbSucker = 0
	this.initLevel(0)
End Sub

Destructor game()
	Screen 0
	Print "Thanks to play with Lander by redcrab"
	Sleep 3000
End Destructor

function pppc(i As Integer) As String
	Dim s As String
	Dim j As Integer
	For j = 1 To i
		s = s + " "
		
	Next
	Return s
End Function

Sub game.showmessage()
	Dim i As Integer
	Dim fi As Integer
	Dim m(0 To 100) As String
	Dim l As Integer
	Dim t As double
	Dim lmax As Integer
	lmax = 0
	fi = FreeFile
	i = Open("m"+Str(this.safeland)+".lvl" For Input As #fi)
	l = 0
	If i = 0 Then
		While (Not Eof(fi)) And l <= UBound(m)
			Input #fi,m(l)
			If Len(m(l)) > lmax Then lmax = Len(m(l)) 
			l + = 1
		Wend
		lmax +=1
		Close #fi
		Cls
		text.scalerot(4*KW,0)
		For i = 0 To l-1 
			m(i) = " "+m(i) + pppc(Lmax-Len(m(i)))
			text.DrawText(m(i),G_WIDTH/2,(i*9+6)*KH,10)
		Next
		Flip
		Do 
		Loop While InKey <> ""
		Sleep 100,1
		t = timer
		Do 
			Sleep 1,1
		Loop While InKey="" And Timer-t < 25
	EndIf	
End Sub

Sub game.Draw()
	Dim As Integer i,lb,ub
	Dim aLife As lander
	Dim speed As Integer
	Dim destx As Double
	Dim desty As Double
	Dim m As string
	Cls
	' Lanscape
	this.scene.Draw
	' Lander
	this.ship.Draw
	' Sucker
	For i = 0 To this.nbSucker-1 
		this.Sucker(i).draw()
	Next
	'Safe landing
	'boardtxt.DrawText(" Safe "+Str( this.safeland),0,234*KH,10)
	'Score
	If this.status <> GS_EDIT And this.status <> GS_EDIT_TEXT Then
		boardtxt.DrawText(" & "+Str(this.safeland)+"  # "+Str(this.score),0,234*KH,10)
	else	
		If this.scene.allowtakeoff=0 Then
			m = "L"
		Else
			m="T"
		EndIf
		boardtxt.DrawText(" & "+Str(this.safeland)+" ("+Str(this.sublevelx)+","+Str(this.sublevely)+")"+m,0,234*KH,10)
	EndIf

	'Life
	If this.status <> GS_EDIT And this.status <> GS_EDIT_TEXT then
		aLife.size = 5
		aLife.status = LS_NORMAL
		aLife.location.y = 6
		For i = 1 To this.life
			aLife.location.x = 88+25+(i-1)*12
			aLife.angle = this.tic*PI/180
			aLife.Draw
		Next i
	End If
	
	'Arrow to show possible direction
	If this.scene.go_up Then
		boardtxt.scaleRot(boardtxt.size,0)
		boardtxt.DrawText("%",88*KW,229*KH,10)
	EndIf
	If this.scene.go_down Then
		boardtxt.scaleRot(boardtxt.size,PI)
		boardtxt.DrawText("%",88*KW,235*KH,10)
	EndIf
	If this.scene.go_left Then
		boardtxt.scaleRot(boardtxt.size,-PI/2)
		boardtxt.DrawText("%",85*KW,232*KH,10)
	EndIf
	If this.scene.go_right Then
		boardtxt.scaleRot(boardtxt.size,PI/2)
		boardtxt.DrawText("%",91*KW,232*KH,10)
	EndIf

		
	'Fuel	
	boardtxt.scaleRot(boardtxt.size, this.tic*PI/180)
	boardtxt.DrawText(" ~",150*KW,235*KH,10,0)
	boardtxt.scaleRot(boardtxt.size, 0)
	Line (161*KW,231*KH)-((161+this.ship.fuel)*KW,239*KH),10,BF
	Line (161*KW,231*KH)-(261*KW,239*KH),10,B
	
	'Speed
	If this.ship.speed.y < 0 Then
		speed  = Int((this.ship.speed.x^2+this.ship.speed.y^2)*1000)
		If Int(Abs(this.ship.angle/(PI/180))) >= 10 Then speed = 100
		If speed > 100 Then speed = 100
		Line(265*KW,231*KH)-((265+speed/2)*KW,239*KH),10,bf
	EndIf
	Line(265*KW,231*KH)-((265+60/2)*KW,239*KH),10,B
	Line(265*KW,231*KH)-((265+50)*KW,239*KH),10,B
	
	
	Select Case this.status
					
	Case GS_START		
	' START
		If this.tic2 < 200 Then
			text.DrawText(this.msg(this.safeland),G_WIDTH/2,120*KH*(this.tic2-10)/190, 10)	
		else
		   If this.tic2 > 500 Then
		   	text.scaleRot(4*KW,Sin((this.tic2-500)*PI/180/2)*3.14159/4)
		   EndIf
   		text.DrawText(this.msg(this.safeland),G_WIDTH/2,120*KH, 10)
		   text.scalerot(4*KW,0)
		End if

	Case GS_INTRO	
	' INTRO
		If this.tic2 <= 200 Then
			bigtxt.scaleRot(this.tic2/200*5*KW,this.tic2/100*PI)
		EndIf
		bigtxt.DrawText("$ LANDER by REDCRAB v 1.4.1 $",G_WIDTH/2,95*KH,10)
		If this.tic2 >=200 then		text.DrawText("Left   : Turn left " ,G_WIDTH/2,(105+9)*KH,10)
		If this.tic2 >=230 then		text.DrawText("Right  : Turn right",G_WIDTH/2,(105+18)*KH, 10)
		If this.tic2 >=260 then		text.DrawText("Up     : Thrust    "  ,G_WIDTH/2,(105+27)*KH, 10)
		If this.tic2 >=290 then		text.DrawText("Space  : Pause     ",G_WIDTH/2,(105+36)*KH, 10)
		If this.tic2 >=320 Then		text.DrawText("Escape : Quit      ",G_WIDTH/2,(105+45)*KH, 10)
		If this.tic2 >=320 And this.bestscore <> 0 Then 	text.DrawText("Best score "+Str(this.bestscore)+" with "+Str(this.bestsafeland)+" safe landing",G_WIDTH/2,(105+62)*KH, 10)
		If this.tic2 >=320 And this.bestscore = 0 Then  text.DrawText("No Best Score Yet !",G_WIDTH/2,(105+62)*KH, 10)
		If this.tic2 <= 350 And this.tic2 >= 200 Then
			text.DrawText(this.msg(this.safeland),G_WIDTH/2,(110+72)*KH*(this.tic2-300)/(350-300), 10)
		Else
			If  this.tic2 >= 350 Then text.DrawText(this.msg(this.safeland),G_WIDTH/2,(110+72)*KH, 10)
		EndIf
	
	Case GS_PAUSE
	' PAUSE	
		If this.tic mod 60 < 30 Then
			bigtxt.scaleRot(5*KW,0)
			bigtxt.DrawText ("Pause" ,G_WIDTH/2,120*KH,10)
		EndIf

	Case GS_RUN
	' RUNNING 	
	  If this.ship.fuel <=0 then
		If this.tic mod 30 < 15 Then
			bigtxt.scaleRot(5*KW,0)
			bigtxt.DrawText ("NO ENERGY !" ,G_WIDTH/2,120*KH,10)
		EndIf
	  End If
	  
	Case GS_CRASHED
	' CRASHED
	' "Life request" animation
		If this.tic2 <= 60 Then
			aLife.location.x = 88+25+(this.life)*12
			aLife.angle = this.tic*PI/180
			aLife.Draw
		EndIf
		If this.tic2 = 60 Then
			this.LoadLevel(this.safeland,0,0)
		EndIf
		If this.tic2  > 60 Then
			destx = this.scene.startlocation.x
			desty = this.scene.startlocation.y
			If this.tic2 < 60+200 Then
				aLife.location.x = (destx-(88+25+(this.life)*12))/200.0*(this.tic2-60)+88+25+(this.life)*12
				alife.location.y = (desty-6.0)/200.0*(this.tic2-60)+6
				alife.size = (7.0-5.0)/200.0*(this.tic2-60)+5
				aLife.angle = this.tic*PI/180
			Else
				smalltxt.DrawText("Ready to land",destx*KW,((240-desty)+7+4)*KW,10)
				aLife.location.x = destx
				alife.location.y = desty
				alife.size = 7
				aLife.angle = this.tic*PI/180
			End If
			aLife.Draw
		End If
		If this.tic2 < 200 Then
			'text2.boom = (10/200.0*this.tic2)-9
			If this.tic2 Mod 4 = 0 Then 
				text2.scaleRot(15*KW+Rnd*2,Rnd*0.2-0.1)
			End If
		Else
			text2.boom = 1
		End If
		
		text2.DrawText ("CRASH!" ,G_WIDTH/2,120*KH,10)
		text2.boom = 1

	Case GS_GAMEOVER	
	' GAME OVER
  		
		If this.tic2 < 200 Then
			text2.boom = 10-(9/200.0*this.tic2)
			text2.scalerot(5*KW*this.tic2/60,0)
		EndIf
		text2.DrawText ("GAME OVER" ,G_WIDTH/2,120*KH,10)
		text2.boom = 1

	Case GS_FINISH	
	' FINISH THE GAME
		If this.tic2 < 200 Then
			text2.boom = 10-(9/200.0*this.tic2)
			text2.scalerot(5*KW*this.tic2/60,0)
		EndIf
		text2.DrawText ("YOU WIN" ,G_WIDTH/2,120*KH,10)
		text2.boom = 1
	
	Case GS_LANDED
	' LANDED
		If this.tic2 <= 180 Then
			text2.scalerot(5*KW*this.tic2/60,this.tic2*1.0/90*PI)
		End If
		text2.DrawText (this.scene.landedmsg ,G_WIDTH/2,120*KH,10)
		
	Case GS_EDIT,GS_EDIT_TEXT		
	' LEVEL editor
	If MultiKey(FB.SC_F1) Then
		Cls
		Locate 2,1
		Color 10
		Print "EDITOR COMMAND"
		Print "--------------"
		Print " F1 .....................: This help"
		Print " F2 .....................: Save current Level"
		Print " F3 .....................: Load current Level"
		Print " F4 .....................: Generate a Lanscape (current level)"
		Print " F5 .....................: Allow/disallow launch after landing (end level anim)"
		Print " F6 .....................: Remove landpad"
		Print " F7 .....................: Remove fuelpad"
		Print " F10 ....................: Enter / Leave Level Editor"
		Print " INSERT .................: Add an Energy Sucker at mouse position"
		Print " DELETE .................: Remove Last Added Energy Sucker"
		Print " HOME/END ...............: + / - Gravity"
		Print " PGUP/DOWN ..............: + / - Energy"
		Print " SPACE ..................: Place Land Pad at mouse position"
		Print " SHIFT + SPACE ..........: Place Energy Reload Pad at mouse position"
		Print " BACK-SPC ...............: Enter Title Edit Mode"
		Print " ENTER ..................: Valid Title(Edit Mode)"
		Print " LEFT/RIGHT/UP/DOWN .....: Move Lander"
		Print " LEFT Sft+LEFT/RIGHT ....: Rotate Lander"
		Print " CTRL+UP/DOWN/LEFT/RIGHT : Shift landscape "
		Print " t,b,l,r ................: allow/disallow sublevel top/bottom/left/right"
		Print " T,B,L,R ................: move to sublevel top/bottom/left/right"
		Print " + / - ..................: Change Level Up/Down"
		Print " LEFT MOUSE BUTTON ......: Draw Ground (slowy please to avoid picks)"
		Print " RIGHT MOUSE BUTTON .....: Draw Sky (slowy please to avoid picks)"
		Print " "
		Print " Auto-save level when leaving Editor (F10)"	
		Print " Auto-load level when entering Editor (F10)"
		Print " "
		Print " TIP : You may use Landscape generator (F4) and use the land pad command  (keep space key down) and move mouse to have a quick landscape design"
		Print ""
		Print "Press Any Key to continue"	
		Flip
		Sleep
		Sleep 100,1
		While InKey <> "" : Wend
		Sleep 100,1
	EndIf
	If this.scene.go_up Then
		Line (0,0)-(320*KW-1,5*KH),09,B,&hFF00
	EndIf
	If this.scene.go_down Then
		Line (0,(240-20)*KH)-(320*KW,(240-20-5)*KH),09,B,&hFF00
	EndIf
	If this.scene.go_left Then
		Line (0,0)-(5*kw,(240-20)*KH),9,B,&hFF00
	EndIf
	If this.scene.go_right Then
		Line ((320-5)*KW,0)-((320)*KW-1,(240-20)*KH),9,B,&hFF00
	EndIf
	boardtxt.DrawText("G",80*KW,234*KH,10)
	   Line (88*KW,230*KH)-((88+50)*KW,239*KH),10,B
	   Line (88*KW,230*KH)-((88+(this.scene.gravity*2000))*KW,239*KH),10,BF
	 	boardtxt.DrawText("LEVEL EDITOR",246*KW,226*KH,9)
	 	If this.status = GS_EDIT_TEXT Then 
	 		Dim pst As String
		 	If this.tic Mod 30 <15 Then
		 		pst = " "
		 	Else
		 		pst = "_"
		 	EndIf
		 	pst = this.msg(this.safeland)+pst
		   text.scalerot(4*KW,0)
  			text.DrawText(pst,G_WIDTH/2,120*KH, 10)
	 	End If
	 	
		boardtxt.DrawText("X",this.xm*KW,this.ym*KH,10)
	End Select
	
	' Screen sync delay and show drawing result
	screensync
	Flip
End Sub


function game.SaveLevel(lvl As Integer) As Integer
	Return SaveLevel(lvl,-100,-100)	
End Function

function game.SaveLevel(lvl As Integer, sublvlx As Integer, sublvly As integer) As Integer
	Dim fi As Integer
	Dim i As Integer
	Dim vv As String
	fi = FreeFile
	vv = "."+Str(sublvlx)+"."+Str(sublvly)
	If sublvlx <=-100 And sublvly <=-100 Then vv = ".0.0"
	i = Open("l"+Str(lvl)+vv+".lvl" For Output As #fi)
	If i = 0 Then
		' level format version
		Print #fi,"version=1"
		' put ground landscape
		For i = 0 To 319
			Print #fi,this.scene.sky(i)
			Print #fi,this.scene.ground(i)
		Next i 
		Print #fi,this.scene.go_up
		Print #fi,this.scene.go_down
		Print #fi,this.scene.go_left
		Print #fi,this.scene.go_right
		Print #fi,this.scene.inverse
		Print #fi,this.scene.allowtakeoff
		Print #fi,this.scene.landedmsg
		
		' put pad location
		Print #fi,this.scene.padlocation.x
		Print #fi,this.scene.padlocation.y
		' put fuel location
		Print #fi,this.scene.fuellocation.x
		Print #fi,this.scene.fuellocation.y
		'put gravity
		Print #fi,this.scene.gravity
		' put lander location
		Print #fi,Int(this.ship.location.x*1000)/1000
		Print #fi,Int(this.ship.location.y*1000)/1000
		
		' put lander speed
		Print #fi,Int(this.ship.speed.x*100000)/100000
		Print #fi,Int(this.ship.speed.y*100000)/100000
		' put lander angle
		Print #fi,this.ship.angle
		' put fuel
		Print #fi,this.ship.fuel
		
		
		' put enemies quantity of "Energy sucker"
		Print #fi,this.nbSucker
		For i = 0 To this.nbSucker -1
			' put enemy location
			Print #fi,this.Sucker(i).location.x
			Print #fi,this.Sucker(i).location.y
		Next
		'put message of level
		If (lvl>= LBound(this.msg) And lvl <= UBound(this.msg)) then
			Print #fi,this.msg(lvl)
		Else
			Print #fi,"Msg"+Str(lvl)
		End If
		
		Close #1
		Return 1
	Else
		Return 0
	EndIf
	Return 0
End Function

function game.LoadLevel(lvl As Integer) As Integer
	Return Loadlevel(lvl,-100,-100)
End Function

function game.LoadLevel(lvl As Integer, sublvlx As Integer,sublvly As Integer) As Integer
	Dim fi As Integer
	Dim i As Integer
	Dim vv As string
	fi = FreeFile
	vv = "."+Str(sublvlx)+"."+Str(sublvly)
	If sublvlx <= -100 And sublvly <=-100 Then vv =".0.0"
	i = Open("l"+Str(lvl)+vv+".lvl" For Input As #fi)
	If i = 0 Then
		Input #fi,vv
		If vv <> "version=1" Then
			Close #fi
			i = 1
		EndIf
	EndIf
	If i = 0 Then
		' get ground landscape
		For i = 0 To 319
			Input #fi,this.scene.sky(i)
			Input #fi,this.scene.ground(i)
		Next i 
		Input #fi,this.scene.go_up
		Input #fi,this.scene.go_down
		Input #fi,this.scene.go_left
		Input #fi,this.scene.go_right
		Input #fi,this.scene.inverse
		Input #fi,this.scene.allowtakeoff
		Input #fi,this.scene.landedmsg
		
		' get pad location
		Input #fi,this.scene.padlocation.x
		Input #fi,this.scene.padlocation.y
		' get fuel location
		Input #fi,this.scene.fuellocation.x
		Input #fi,this.scene.fuellocation.y

		' get gravity
		Input #fi,this.scene.gravity
		If (sublvlx<=-100 And sublvly <=-100) Or this.status = GS_EDIT Or this.status = GS_EDIT_TEXT then
			' get lander location
			Input #fi,this.ship.location.x
			Input #fi,this.ship.location.y
			this.scene.startlocation.x = this.ship.location.x
			this.scene.startlocation.y = this.ship.location.y
			' get lander speed
			Input #fi,this.ship.speed.x 
			Input #fi,this.ship.speed.y
			
			' get lander angle
			Input #fi,this.ship.angle
			' get fuel
			Input #fi,this.ship.fuel
		Else
			' read ship info to ignore
			Input #fi, vv
			Input #fi, vv
			Input #fi, vv
			Input #fi, vv
			Input #fi, vv
			Input #fi, vv
		End If
		
	'	Print this.ship.angle,this.ship.fuel
	'	Flip
	'	sleep

		' get enemies quantity of "Energy sucker"
		Input #fi,this.nbSucker
		For i = 0 To this.nbSucker -1
			' get enemy location
			Input #fi,this.Sucker(i).location.x
			Input #fi,this.Sucker(i).location.y
		Next
		'get message of level

		If lvl >=LBound(this.msg) And lvl <=UBound(this.msg) then
				Input #fi,this.msg(lvl)
		End If

'		If sublvlx<=-100 And sublvly<=-100 Then
'		Else
'		End if
		Close #1
		Return 1
	Else
		Return 0
	EndIf
	Return 0
End Function


Sub game.initLevel(level As integer)
	Dim i As Integer
	this.playeract = PA_NOTHING
	this.ship.init
	this.nbSucker = 0
	this.sublevelx = 0
	this.sublevely = 0
	i = this.LoadLevel(level)
	If i = 0 then
	   this.scene.init(level)
	   If this.safeland > 3 Then
	   	this.nbSucker = Int(this.safeland / 3.0)
	   	this.nbSucker = IIf(this.nbSucker>UBound(this.Sucker)+1,UBound(this.Sucker),this.nbSucker)
	   	For i = 0 To this.nbSucker-1
	   		this.Sucker(i).location.x = 160 + 160- this.scene.padlocation.x+Rnd*30-15
	   		If Abs(this.Sucker(i).location.x - this.scene.padlocation.x) < 100 Then
	   			this.Sucker(i).location.x*=2.0
	   			If this.Sucker(i).location.x > 319 Then this.Sucker(i).location.x = 319
	   		endif
	   		this.Sucker(i).location.y = this.scene.ground(Int(this.Sucker(i).location.x))+20+ (220- this.scene.ground(Int(this.Sucker(i).location.x)))*rnd
	   	Next i
	   EndIf
	End If
	this.ship.location.x = this.scene.startlocation.x
	this.ship.location.y = this.scene.startlocation.y
End Sub

Sub game.EnergySuckerAction
	Dim i As Integer
	Dim As Double xs,ys,xm,ym,d
	Dim level As Integer
	level = iif(this.safeland>10,10,this.safeland)
	level += 4
	
	xs = this.ship.location.x
	ys = this.ship.location.y
	If this.status = GS_RUN then
		For i = 0 To this.nbSucker-1
			If this.Sucker(i).status = SS_NORMAL Then
				rs.setDefault((i Mod 4)+2,0,(this.Sucker(i).tic Mod 60)/2)
				rs.sound (60-(this.Sucker(i).tic Mod 60))*2+80,100,0
				xm = this.Sucker(i).location.x
				ym = this.Sucker(i).location.y
				d = Sqr((xs-xm)^2+(ys-ym)^2)
				If d < (this.ship.size+this.Sucker(i).size) Then
					If (this.tic+i) Mod 10 = 0 Then
						If this.ship.fuel > 0 Then
							this.ship.fuel -=1
							rs.setDefault((i Mod 4)+2,15,255)
							rs.sound 30,250,0
						EndIf 
					EndIf
				EndIf
				If d*0 < 10*level Then 
					If Abs(xs-xm)< 0.0001 Then xm+=0.001
					this.Sucker(i).angle = ATan2((ys-ym),(xs-xm))
					this.Sucker(i).speed.x = Cos(this.Sucker(i).angle)*7/60.0
					this.Sucker(i).speed.y = sin(this.Sucker(i).angle)*7/60.0
				EndIf

				If this.Sucker(i).location.y-this.scene.ground(Int(this.Sucker(i).location.x))+10 >3 Then  
					this.Sucker(i).location.x += this.Sucker(i).speed.x
				EndIf
				this.Sucker(i).location.y += this.Sucker(i).speed.y
				If this.Sucker(i).location.y < this.scene.ground(Int(this.Sucker(i).location.x))+10 Then
					this.Sucker(i).location.x -= this.Sucker(i).speed.x
					this.Sucker(i).location.y -= this.Sucker(i).speed.y
					this.Sucker(i).location.y += 0.1'Abs(this.Sucker(i).speed.y)
					'this.Sucker(i).location.y = this.scene.ground(Int(this.Sucker(i).location.x))+20
			EndIf
			
				If this.Sucker(i).location.y-this.scene.sky(Int(this.Sucker(i).location.x))-10 <-3 Then  
					this.Sucker(i).location.x += this.Sucker(i).speed.x
				EndIf
				this.Sucker(i).location.y += this.Sucker(i).speed.y
				If this.Sucker(i).location.y > this.scene.sky(Int(this.Sucker(i).location.x))-10 Then
					this.Sucker(i).location.x -= this.Sucker(i).speed.x
					this.Sucker(i).location.y -= this.Sucker(i).speed.y
					this.Sucker(i).location.y -= 0.1 'Abs(this.Sucker(i).speed.y)
					'this.Sucker(i).location.y = this.scene.ground(Int(this.Sucker(i).location.x))+20
				EndIf
			EndIf
		Next i
	End If
	
End Sub


Sub game.MainLoop()
	dim st as String
	Dim As Integer speed,angle,fi,i,xxm,yym
	Dim As Integer v1,v2
	rs.setDefault(0,2,128)
	showmessage()
	Do
		this.tic +=1
		this.tic2 +=1
		st = InKey
		rs.tick
		' Player action
		this.playeract = PA_NOTHING
		' Lander sound diming
		v1 = rs._channel(1).volume
		v2 = rs._channel(1).frequency
		'If tic Mod 2 = 0 Then	
			v1 = v1 - 2
			v2 = v2-2
			If v2 < 30 Then v2 = 30
			If v1 < 0 Then v1 = 0
			If v1 > 40 Then v1 = 40
		'End If
		rs.keyonoff(1,15,v2,v1,100)
		
		If MultiKey(FB.SC_LEFT) Then this.playeract = PA_LEFT
		If MultiKey(FB.SC_1) Then this.playeract = PA_LEFT
		If MultiKey(FB.SC_RIGHT) Then this.playeract = PA_RIGHT
		If MultiKey(FB.SC_3) Then this.playeract = PA_RIGHT
		If MultiKey(FB.SC_UP) Then this.playeract = PA_THRUST
		If MultiKey(FB.SC_2) Then this.playeract = PA_THRUST
		if Multikey(FB.SC_SPACE) then this.playeract = PA_PAUSE
		If MultiKey(FB.SC_ESCAPE) Then this.playeract = PA_QUIT
		If MultiKey(FB.SC_F10) And this.status <> GS_EDIT And this.tic >60 Then
#If  __FB_DEBUG__=0
			If Left(Trim(UCase(Command)),Len("-EDIT"))="-EDIT" Then
#endif				
				this.status = GS_EDIT
				this.ship.speed.x = 0
				this.ship.speed.y = 0
				this.ship.status = LS_NORMAL
				this.tic = 0
				this.tic2 = 0
				this.LoadLevel(this.safeland,this.sublevelx,this.sublevely)
#If __FB_DEBUG__ = 0
			End If
#endif			
		EndIf 
		
#Ifdef CHEAT		
		' Cheat keys (Debugging purpose)		
		'If st="-" Then this.ship.status = LS_CRASH
		'If st="+" Then this.ship.status = LS_NORMAL
		If st="*" Then this.ship.fuel = 100
		If st="/" Then 
			this.tic = 0
			this.life = 0
			this.tic2 = 0
			this.status = GS_GAMEOVER
		End If
#endif

		'Enemy action
		this.EnergySuckerAction

		If this.status = GS_GAMEOVER Or this.status = GS_FINISH then
	   	If this.bestscore<this.score Then
		   	fi  = freefile
		  		this.bestscore = this.score
		  		this.bestsafeland = this.safeland
		  		If Open("lander.sco" For Output As #fi)=0 Then
					print #fi,this.bestscore
					print #fi,this.bestsafeland
					Close #fi
		  		EndIf
	   	EndIf					
		EndIf

		' Process player action
		If this.status = GS_START Then
			If this.tic = 1 Then
				rs.setDefault(0,1,64)
				rs.sound 220,300.1,2
				rs.setDefault(0,1,0)
				rs.sound 0,300.1,2
				rs.setDefault(0,1,64)
				rs.sound 220,300.1,2
				rs.setDefault(0,1,128)
				rs.sound 440,400.1,2
			EndIf
			this.ship.location.x = this.scene.startlocation.x
			this.ship.location.y = this.scene.startlocation.y				
			If st <> "" And this.tic > 30 Then
				this.status = GS_RUN
				this.tic = 0
			End If
		EndIf

		If this.status=GS_INTRO  Then
			If this.tic = 1 Then
				rs.setDefault(0,3,128)
				rs.sound -60-6,700.2,2
				rs.sound -60-7,800.2,2
				rs.setDefault(0,3,128)
				rs.sound -60-3,1400.1,2
			EndIf
			this.ship.location.x = this.scene.startlocation.x
			this.ship.location.y = this.scene.startlocation.y				
			If st <> "" And this.tic > 30 Then 
				this.status = GS_RUN
				this.tic = 0
			EndIf
		EndIf

		If this.status=GS_LANDED And this.tic > 120 Then
			If this.ship.fuel <100 And this.tic Mod 3 = 0 Then
				this.ship.fuel+=1
			EndIf
			If st <> "" or this.ship.location.y > 280 Or this.tic > 60*10 Then 
				this.showmessage
				this.status = GS_START
				this.initLevel(this.safeland)
				this.tic = 0
				this.tic2 = 0
			End if
		EndIf
		
		If this.status=GS_CRASHED And this.tic > 120 Then
			If st <> "" Or this.tic2 > 260 + 180 Then 	
				this.status = GS_START
				this.ship.init
				this.initLevel(this.safeland)
				this.tic = 0
				this.tic2 = 0
			End if
		EndIf
		
		If this.status=GS_GAMEOVER And this.tic > 120 Then
			If st <> "" Or this.tic > 800 Then 	
				this.init()
				this.tic = 0
				this.tic2 = 0
			End if
		EndIf

		If this.status=GS_FINISH And this.tic > 120 Then
			If st <> "" Or this.tic > 800 Then 	
				this.init()
				this.tic = 0
				this.tic2 = 0
			End if
		EndIf
		
		if this.status = GS_PAUSE And this.tic > 60 Then
			if this.playeract = PA_PAUSE then
				this.status = GS_RUN
				while inkey<> "" : sleep 1,1 :wend
				sleep 100,1
				this.playeract = PA_NOTHING
			end if
		end If
		
		If this.status = GS_EDIT Or this.status = GS_EDIT_TEXT Then
			'If this.safeland = 0 Then this.safeland = 1
			GetMouse xxm,yym,,this.bm
			this.xm = xxm/KW*1.0
			this.ym = yym/KH*1.0
			this.scene.startlocation.x = this.ship.location.x
			this.scene.startlocation.y = this.ship.location.y
			If MultiKey(FB.SC_CONTROL)=0 then
				If MultiKey(FB.SC_UP) And MultiKey(FB.SC_LSHIFT)=0 Then this.ship.location.y+=1/KH
				If MultiKey(FB.SC_DOWN) And MultiKey(FB.SC_LSHIFT)=0 Then 	this.ship.location.y-=1/KH
				If MultiKey(FB.SC_LEFT) And MultiKey(FB.SC_LSHIFT)=0 Then this.ship.location.x -=1/KW
				If MultiKey(FB.SC_RIGHT) And MultiKey(FB.SC_LSHIFT)=0 Then this.ship.location.x +=1/KW
				If MultiKey(FB.SC_LEFT) And MultiKey(FB.SC_LSHIFT) Then 
					this.ship.angle -= PI/100
					If this.ship.angle < -PI Then this.ship.angle += 2*PI
				End if
				If MultiKey(FB.SC_RIGHT) And MultiKey(FB.SC_LSHIFT) Then
					this.ship.angle += PI/100
					If this.ship.angle > PI Then this.ship.angle -= 2*PI
				End if
			Else
				If MultiKey(FB.SC_UP) Then
					For i = LBound(this.scene.ground) To UBound(this.scene.ground)
						this.scene.ground(i)+=1
						this.scene.sky(i)+=1
					Next
					For i = lbound(this.Sucker) To UBound(this.sucker)
						this.Sucker(i).location.y+=1
					Next
					this.scene.padlocation.y+=1
					this.scene.fuellocation.y+=1
					this.ship.location.y+=1
				End If
				If MultiKey(FB.SC_DOWN) Then
					For i = LBound(this.scene.ground) To UBound(this.scene.ground)
						this.scene.ground(i)-=1
						this.scene.sky(i)-=1
					Next
					For i = lbound(this.Sucker) To UBound(this.sucker)
						this.Sucker(i).location.y-=1
					Next
					this.scene.padlocation.y-=1
					this.scene.fuellocation.y-=1
					this.ship.location.y-=1
				End If
				If MultiKey(FB.SC_LEFT) Then
					Dim As Double tg,ts
					tg = this.scene.ground(UBound(this.scene.ground))
					ts = this.scene.sky(UBound(this.scene.ground))
					For i = UBound(this.scene.ground) To LBound(this.scene.ground)+1 Step -1
						this.scene.ground(i)=this.scene.ground(i-1)
						this.scene.sky(i)=this.scene.sky(i-1)
					Next
					this.scene.ground(LBound(this.scene.ground))=tg
					this.scene.sky(LBound(this.scene.ground))=ts
					
					For i = lbound(this.Sucker) To UBound(this.sucker)
						this.Sucker(i).location.x+=1
						If this.Sucker(i).location.x >=320 Then this.Sucker(i).location.x -= 320
					Next
					If this.scene.padlocation.x > -100 Then
						this.scene.padlocation.x+=1
						If this.scene.padlocation.x >=320 Then this.scene.padlocation.x -= 320
					End if	
					If this.scene.fuellocation.x >-100 then
						this.scene.fuellocation.x+=1
						If this.scene.fuellocation.x >=320 Then this.scene.fuellocation.x -= 320
					End if	
					this.ship.location.x+=1
					If this.ship.location.x >= 320 Then this.ship.location.x -= 320
				End If
				If MultiKey(FB.SC_RIGHT) Then
					Dim As Double tg,ts
					tg = this.scene.ground(LBound(this.scene.ground))
					ts = this.scene.sky(LBound(this.scene.ground))
					For i = LBound(this.scene.ground) To UBound(this.scene.ground)-1
						this.scene.ground(i)=this.scene.ground(i+1)
						this.scene.sky(i)=this.scene.sky(i+1)
					Next
					this.scene.ground(UBound(this.scene.ground))=tg
					this.scene.sky(UBound(this.scene.ground))=ts
					
					For i = lbound(this.Sucker) To UBound(this.sucker)
						this.Sucker(i).location.x-=1
						If this.Sucker(i).location.x <0 Then this.Sucker(i).location.x += 320
					Next
					If this.scene.padlocation.x > -100 then
						this.scene.padlocation.x-=1
						If this.scene.padlocation.x <0 Then this.scene.padlocation.x += 320
					End If
					If this.scene.fuellocation.x > -100 then
						this.scene.fuellocation.x-=1
						If this.scene.fuellocation.x <0 Then this.scene.fuellocation.x += 320
					End if	
					this.ship.location.x-=1
					If this.ship.location.x <0 Then this.ship.location.x += 320
				End If
			End If
			
			If MultiKey(FB.SC_F10) And this.tic > 60 Then 
				this.status = GS_RUN
				this.tic = 0
				this.saveLevel(this.safeland,this.sublevelx,this.sublevely)
			EndIf
			' DRAW GROUND
			If this.bm = 1 Then 
				this.scene.ground(Int(this.xm))=Int(240-this.ym)
				If this.scene.ground(Int(this.xm)) <  20 Then this.scene.ground(Int(this.xm)) = -20
				If this.scene.ground(Int(this.xm)) > 220 Then this.scene.ground(Int(this.xm)) = 220
				If this.scene.ground(Int(this.xm)) > this.scene.sky(Int(this.xm)) Then
					this.scene.sky(Int(this.xm)) = this.scene.ground(Int(this.xm))
				End if		
			End If
			' DRAW SKY
			If this.bm = 2 Then 
				this.scene.sky(Int(this.xm))=Int(240-this.ym)
				If this.scene.sky(Int(this.xm)) <  20 Then this.scene.sky(Int(this.xm)) = 20
				If this.scene.sky(Int(this.xm)) > 220 Then this.scene.sky(Int(this.xm)) = 250
				If this.scene.ground(Int(this.xm)) > this.scene.sky(Int(this.xm)) Then
					this.scene.ground(Int(this.xm)) = this.scene.sky(Int(this.xm))
				End if		
			End If
			If this.status = GS_EDIT_TEXT Then ' change text
				If st <> "" then
					If Asc(st)> 31 And Asc(st)<255 Then
						msg(this.safeland) += st
					EndIf
					If Asc(st)= 8 Then msg(this.safeland) = Left(msg(this.safeland),Len(msg(this.safeland))-1)
					If st = Chr(13) Then this.status = GS_EDIT
				End if
			Else
				' SWITCH TO EDIT LEVEL MESSAGE MODE
				If st = Chr(8) Then this.status = GS_EDIT_TEXT
				' INSERT LAND PAD
				If st=" " And  MultiKey(FB.SC_LSHIFT)=0 Then 
					If this.xm <10 Then this.xm = 10
					If this.xm>309 Then this.xm = 309
					this.scene.padlocation.x = this.xm
					this.scene.padlocation.y = 240-this.ym
					For i = this.xm - 10 To this.xm + 10
						this.scene.ground(i) = this.scene.padlocation.y 
					Next i 
				EndIf
				' INSERT FUEL PAD
				If st=" " And MultiKey(FB.SC_LSHIFT) Then 
					If this.xm <10 Then this.xm = 10
					If this.xm>309 Then this.xm = 309
					this.scene.fuellocation.x = this.xm
					this.scene.fuellocation.y = 240-this.ym
					For i = this.xm - 10 To this.xm + 10
						this.scene.ground(i) = this.scene.fuellocation.y 
					Next i 
				EndIf
				' ENABLE/DISBALE UP/DOWN/LEFT/RIGHT PASS
				If st="t" Then
					this.scene.go_up = iif(this.scene.go_up=0,1,0)
				end If
				If st="b" Then
					this.scene.go_down = iif(this.scene.go_down=0,1,0)
				end If
				If st="l" Then
					this.scene.go_left = iif(this.scene.go_left=0,1,0)
				end If
				If st="r" Then
					this.scene.go_right = iif(this.scene.go_right=0,1,0)
				end If
				' CHANGE SUBLEVEL 
				If st="T" Then
					this.sublevely +=1
				end If
				If st="B" Then
					this.sublevely -=1
				end If
				If st="L" Then
					this.sublevelx -=1
				end If
				If st="R" Then
					this.sublevelx +=1
				end If
				' SWITH TO ONE LEVEL MORE
				If st = "+" And this.safeland <22 Then
					this.safeland+=1
				EndIf
				' SWITH TO ONE LEVEL LESS
				If st = "-" And this.safeland >=0 Then
					this.safeland-=1
				EndIf
				' SAVE LEVEL 
				If MultiKey(FB.SC_F2) And this.tic > 60 Then
					this.tic = 0
					this.savelevel(this.safeland,this.sublevelx,this.sublevely)
				EndIf
				' LOAD LEVEL
				If MultiKey(FB.SC_F3) And this.tic > 60 Then
					this.tic = 0
					this.savelevel(999)	
					this.ship.init
					If this.sublevelx = 0 And this.sublevely = 0 Then
						If this.Loadlevel(this.safeland)=0 Then
				 			this.loadlevel(999)
				 		EndIf
					Else
						If this.Loadlevel(this.safeland,sublevelx,sublevely)=0 Then
					 		this.loadlevel(999)
					 	EndIf
					EndIf
				EndIf
				' ALLOW / DISALLOW TAKE OFF after landing
				If MultiKey(FB.SC_F5) And this.tic > 60 Then
					this.scene.allowtakeoff = IIf(this.scene.allowtakeoff=0,1,0)
					this.tic = 0
				End If
				' REMOVE LANDPAD
				If MultiKey(FB.SC_F6) Then
					this.scene.padlocation.x = -100
				End If
				' REMOVE FUELPAD
				If MultiKey(FB.SC_F7) Then
					this.scene.fuellocation.x = -100
				End If
				
				' GENERATE LEVEL
				If MultiKey(FB.SC_F4) And this.tic > 3 Then
					this.tic = 0
					this.scene.init(this.safeland)
				EndIf
				' ADD ONE MORE ENERGY SUCKET AT MOUSE POSITION
				If MultiKey(FB.SC_INSERT) And this.tic > 15 Then
					this.tic = 0
					this.nbsucker+=1
					this.Sucker(this.nbsucker-1).location.x = this.xm
					this.Sucker(this.nbsucker-1).location.y = 240-this.ym
				End If
				' REMOVE LAST ADDED ENERGY SUCKER
				If MultiKey(FB.SC_DELETE) And this.tic > 15 Then
					this.tic = 0
					If this.nbSucker > 0 Then this.nbSucker -=1		
				End If
				' ADD MORE FUEL
				If MultiKey(FB.SC_PAGEUP) And this.tic > 3 Then
					this.tic = 0
				   If this.ship.fuel <100 Then this.ship.fuel+=1	
				End if	
				' REMOVE FUEL
				If MultiKey(FB.SC_PAGEDOWN) And this.tic > 3 Then
					this.tic = 0
				   If this.ship.fuel >0 Then this.ship.fuel-=1	
				End if	
				' ADD MORE GRAVITY
				If MultiKey(FB.SC_HOME) And this.tic > 3 Then
					this.tic = 0
					this.scene.gravity = Int(this.scene.gravity/0.0025+0.5)*0.0025
					If this.scene.gravity <0.025 Then this.scene.gravity+=0.0025	
				End if	
				' REMOVE GRAVITY
				If MultiKey(FB.SC_END) And this.tic > 3 Then
					this.tic = 0
					this.scene.gravity = Int(this.scene.gravity/0.0025+0.5)*0.0025
				   If this.scene.gravity >0 Then this.scene.gravity-=0.0025	
				End if	
			EndIf
		EndIf

		If this.status = GS_RUN Then
			If this.ship.fuel <=0 Then
				i = this.tic Mod 60
				i = i * 2
				Select Case i
					Case is<60
						rs.setDefault(0,3,i*3)
						rs.Sound 110,100,0
					Case is>=60						
						rs.setDefault(0,3,(i-60)*3)
						rs.Sound 440,100,0
				End Select
				If this.scene.gravity <0.0025 then  this.scene.gravity=0.0025
			EndIf
			
			if this.playeract = PA_PAUSE then
				sleep 100,1
				st = inkey
				this.status = GS_PAUSE
				this.playeract = PA_NOTHING
				this.tic = 0
			end If
			If this.playeract = PA_THRUST Then
				this.ship.status = LS_THRUST
				
			Else
				If this.ship.status <> LS_CRASH Then 
					this.ship.status = LS_NORMAL
				EndIf
			EndIf
			If this.playeract = PA_LEFT And this.ship.fuel> 0 Then this.ship.angle -= PI/100
			If this.playeract = PA_RIGHT And this.ship.fuel> 0 Then this.ship.angle += PI/100
			If this.ship.angle > PI Then
				this.ship.angle -= 2*PI
			EndIf
			If this.ship.angle < -PI Then
				this.ship.angle += 2*PI
			EndIf
			
			If this.playeract = PA_THRUST And this.ship.fuel>0 Then 
				this.ship.speed.x += Sin(this.ship.angle) * 0.03
				this.ship.speed.y += cos(this.ship.angle) * 0.03
				If this.tic Mod 4 = 0 Then this.ship.fuel -= 1
			End If
			this.ship.speed.y-=this.scene.gravity
			this.ship.location.x += this.ship.speed.x
			this.ship.location.y += this.ship.speed.y
			
			If this.ship.location.x < 0 Then
				 If this.scene.go_left Then
					 If LoadLevel(this.safeland,this.sublevelx-1,this.sublevely) Then
				 	   this.sublevelx -=1
				 	   this.ship.location.x = 320-5
				 	 Else
					 	this.ship.location.x = 0
					 	this.ship.speed.x= -this.ship.speed.x /2.0		 	
				 	 End if
				 Else
				 	this.ship.location.x = 0
				 	this.ship.speed.x= -this.ship.speed.x /2.0	
				 End if
			End if
			If this.ship.location.x >= 319 Then
				 If this.scene.go_right Then
					 If LoadLevel(this.safeland,this.sublevelx+1,this.sublevely) Then
				 	   this.sublevelx +=1
				 	   this.ship.location.x = 5
				 	 Else
					 	this.ship.location.x = 319
					 	this.ship.speed.x= -this.ship.speed.x /2.0		 	
				 	 End if
				 Else
				 	this.ship.location.x = 319
				 	this.ship.speed.x= -this.ship.speed.x /2.0
				 End if
			End if	 
			If this.ship.location.y<20 Then 
				 If this.scene.go_down Then
					 If LoadLevel(this.safeland,this.sublevelx,this.sublevely-1) Then
				 	   this.sublevely -=1
				 	   this.ship.location.y = 240-5
				 	 Else
					 	this.ship.location.y = 20
					 	this.ship.speed.y= -this.ship.speed.y /2.0		 	
				 	 End if
				 Else
				 	this.ship.location.y = 20
				 	this.ship.speed.y= -this.ship.speed.y /2.0
				 End if
			End if
			If this.ship.location.y>240 Then
				 If this.scene.go_up Then
					 If LoadLevel(this.safeland,this.sublevelx,this.sublevely+1) Then
				 	   this.sublevely +=1
				 	   this.ship.location.y = 25 
				 	 Else
					 	this.ship.location.y = 239
					 	this.ship.speed.y= -this.ship.speed.y /2.0		 	
				 	 End if
				 Else
				 	this.ship.location.y = 239
				 	this.ship.speed.y= -this.ship.speed.y /2.0
				 End if
			End If
			If  this.ship.location.y <= this.scene.ground(Int(this.ship.location.x))+this.ship.size*0.80 Or _
			    this.ship.location.y >= this.scene.sky(Int(this.ship.location.x))-this.ship.size*0.80 Then
				'this.ship.location.y = this.scene.ground(Int(this.ship.location.x))+this.ship.size*0.80
				speed = Int((this.ship.speed.x^2+this.ship.speed.y^2)*1000)
				angle = Int(Abs(this.ship.angle/(PI/180)))
				If  (speed <=60 And angle <10 And _
					  this.ship.location.y <= this.scene.ground(Int(this.ship.location.x))+this.ship.size*0.80) And _
				    ((this.scene.padlocation.x-10 < this.ship.location.x And _
				     this.scene.padlocation.x+10 > this.ship.location.x) Or _
				     (this.scene.fuellocation.x-10 < this.ship.location.x And _
				     this.scene.fuellocation.x+10 > this.ship.location.x))	     Then
					If (this.scene.padlocation.x-10 < this.ship.location.x And _
				     this.scene.padlocation.x+10 > this.ship.location.x) then				     
						this.safeland+=1
						this.score += this.ship.fuel
						this.ship.status = LS_LANDED
						If this.scene.sky(Int(this.ship.location.x)) <=240 or this.scene.allowtakeoff = 0 Then
							this.ship.status = LS_LANDED_NOSKY
						End if
						If this.safeland>= 23 Then	
							this.status = GS_FINISH
							rs.setDefault(0,3,128)
							For i = 1 To 10 Step 1
								rs.sound(-i-40,200,2)
							Next i
						Else
							rs.setDefault(0,3,128)
							For i = 0 To 11 Step 3
								rs.sound(-i-40,600.2,2)
							Next i
							this.status = GS_LANDED
						End If
						this.tic = 0
					Else
						If this.tic Mod 3 = 0 And this.ship.fuel<100 Then
							this.ship.fuel +=1
							rs.setDefault(0,3,255)
							rs.sound(30,100,0)
						End If
						this.ship.location.y = this.scene.ground(Int(this.ship.location.x))+this.ship.size*0.80 
					End If	
				Else
					this.status = GS_CRASHED
					this.ship.status = LS_CRASH
					If this.life = 0 Then
						this.status = GS_GAMEOVER
						rs.setDefault(0,3,0)
						rs.sound(-1,1000,2)
						rs.setDefault(0,3,128)
						For i = 10 To -2 Step -2
							rs.sound(-i-40,800.2,2)
						Next i
					Else
						this.life-=1
					End If
					this.tic = 0
				EndIf
				this.tic2 = 0
				this.ship.speed.x = 0 
				this.ship.speed.y = 0 
			EndIf
		End If
		
		Sleep 1,1
		this.Draw
	Loop While MultiKey(FB.SC_ESCAPE)=0
	
	
	If this.bestscore<this.score Then
   	fi  = freefile
  		this.bestscore = this.score
  		this.bestsafeland = this.safeland
  		If Open("lander.sco" For Output As #fi)=0 Then
			print #fi,this.bestscore
			print #fi,this.bestsafeland
			Close #fi
  		EndIf
	EndIf		
	this.safeland = 99
	this.showmessage()
End Sub

Sub game.initgfx()
	Screenres G_WIDTH,G_HEIGHT,8,2,FB.GFX_FULLSCREEN
	ScreenSet 0,1
	Cls
End Sub

'**********************
'       Starts !
'**********************
'Randomize timer
Dim Shared agame As game
SetMouse ,,0
agame.MainLoop
Sleep
