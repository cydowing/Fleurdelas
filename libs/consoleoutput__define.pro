Pro consoleOutput__define

; Definition of the data hold by the object
void = {consoleOutput, $
  consoleSetup     :0B, $             ; Execution information output, 0:console,1:file,3:quiet
  logPath          :'', $             ; Path the to log file
  consoleLun       :0B  $             ; Path to the LAS file
  }
  
End



Function consoleOutput::init, _extra = console_options ;, quiet=quiet, file=file, log = log

Compile_opt idl2

; default mode is VERBOSE

if n_elements(console_options) gt 0 then begin

  ntags = n_tags(console_options)
  tags = tag_names(console_options)
  
  for i = 0, ntags-1 do begin
  
    tag = (tag_names(console_options))[i]
      
    case 1 of
      
      strlowcase(tag) eq 'file': begin
        
        logfield = where(strlowcase(tags) eq 'log', /NULL)
        
        if logfield ne !NULL then begin
          self.Logpath = console_options.log
        endif else begin
          self.print, 2, 'No log file name specified...'
          self.print, 2, "The output log name is set to 'idl_console_output.log' and will be located in user's directory..."
          self.LogPath = 'idl_console_output.log'
        endelse
      
        Openw, Lun, self.LogPath, /get_lun
        self.Consolelun = Lun
        self.Consolesetup = 1
        self.print, 2, 'Log file mode enable'
  
      end
      
      ; Enable quiet mode
      strlowcase(tag) eq 'quiet' : begin
        
        self.print, 2, 'Quiet mode enable'
        self.Consolesetup = 2
        
      end
      
      else : self.print, 2, 'Unknown case...'
      
    endcase

  endfor
    
endif

  ; Initializing the object
  Return, 1

End



Pro consoleOutput::cleanup

  Compile_opt idl2
   
  ;ptr_free, self.consoleSetup, self.consoleLun 
    
End  



Pro consoleOutput::help

  help, self, /obj
  return

End


; Function to output information on Log in the GUI
Pro consoleOutput::printLog, pointerState, code, stringText

codeString = ["::","INFO","WARNING","ERROR"]
tempString = string(codeString[code], codeString[0], stringText, format = '(a-7,tr1,a2,tr1,a-255)')
print, tempString
widget_control, (*pointerState).wtLog, set_value=tempString,/APPEND

End



Pro consoleOutput::Progress, code, n, m

  flag = 0

  ;INFO    :: -> code 1
  ;WARNING :: -> code 2
  ;ERROR   :: -> code 3
  codeString = ["::","INFO","WARNING","ERROR"]
  
  case self.Consolesetup of
    0:Print, FORMAT='(%"%s", a-7,tr1,a2,tr1, %"%d job process over %d...",$)', string(13b), codeString[code], codeString[0], n, m
    1:
    2:
  endcase
  
End

; Function to output information on console
Pro consoleOutput::print, code, stringText

flag = 0

;INFO    :: -> code 1
;WARNING :: -> code 2
;ERROR   :: -> code 3
codeString = ["::","INFO","WARNING","ERROR"]


case self.consoleSetup of
0:print, codeString[code], codeString[0], stringText, format = '(a-7,tr1,a2,tr1,a-255)'
1:printf, self.consoleLun, codeString[code], codeString[0], stringText, format = '(a-7,tr1,a2,tr1,a-255)'
2:
endcase



End

; Function to output information on console
Pro consoleOutput::printArray, code, stringArray

flag = 0

;INFO    :: -> code 1
;WARNING :: -> code 2
;ERROR   :: -> code 3
codeString = ["::","INFO","WARNING","ERROR"]

n = n_elements(stringArray)
stringFormat= '(a-7,tr1,a2,tr3,'+string(n)+'(a, :, ", "))'

case self.consoleSetup of
0:print, FORMAT = stringFormat, codeString[code], codeString[0], stringArray
1:printf, self.consoleLun, codeString[code], codeString[0], stringArray, FORMAT = stringFormat
2:
endcase


End


