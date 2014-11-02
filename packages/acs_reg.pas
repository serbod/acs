unit acs_reg;

interface

uses
  Classes, ACS_Audio,
  ACS_CDROM, ACS_AudioMix, ACS_Converters, ACS_Misc, ACS_File, ACS_Filters,
  ACS_Streams, ACS_Indicator, ACS_Mixer, ACS_MultiMix, ACS_VolumeQuery,
  acs_properties, PropEdits
  {$IFDEF FPC}
  ,LResources
  {$ENDIF};

  procedure Register();

implementation

procedure Register();
begin
  RegisterComponents('Audio I/O', [TACSAudioIn, TACSAudioOut, TACSMixer,
  TACSCDIn, TACSInputList, TACSMemoryIn, TACSFileIn, TACSFileOut, TACSStreamIn, TACSStreamOut, TACSNULLOut]);

  RegisterComponents('Audio Processing', [TACSAudioMixer, TACSMultiMixer, TACSSampleConverter, TACSRateConverter,
  TACSMSConverter, TACSAudioProcessor, TACSBWFilter, TACSSincFilter, TACSSoundIndicator, TACSStereoBalance, TACSConvolver, TACSVolumeQuery]);

  { serbod@ }
  RegisterPropertyEditor(TypeInfo(string), TACSAudioIn, 'Driver', TAcsAudioInDriverPropertyEditor);
  RegisterPropertyEditor(TypeInfo(string), TACSAudioOut, 'Driver', TAcsAudioOutDriverPropertyEditor);
end;

initialization
{$IFDEF FPC}
{$i ..\src\resources\acs_reg.lrs}
{$ELSE}
{$R ..\src\resources\resource.dcr}
{$ENDIF}


end.
