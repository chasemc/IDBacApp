#define MyAppName "IDBac"
#define MyAppVersion "0.0.0"
#define MyAppExeName "IDBac.bat"
#define RVersion "3.4.2"
#define IncludeR true
#define PandocVersion "1.19.2.1"
#define IncludePandoc false
#define MyAppPublisher ""
#define MyAppURL ""


[Setup]
AppId = {{U0AVS0HX-MLH5-OBJW-C50E-76KTWEDZI1YH}
AppName = {#MyAppName}
DefaultDirName = {userdocs}\{#MyAppName}
DefaultGroupName = {#MyAppName}
OutputDir = wizard
OutputBaseFilename = setup_{#MyAppName}
SetupIconFile = setup.ico
AppVersion = {#MyAppVersion}
AppPublisher = {#MyAppPublisher}
AppPublisherURL = {#MyAppURL}
AppSupportURL = {#MyAppURL}
AppUpdatesURL = {#MyAppURL}
PrivilegesRequired = none
InfoBeforeFile = infobefore.txt
InfoAfterFile = infoafter.txt
Compression = lzma2/ultra64
SolidCompression = yes

[Languages]
Name: "english"; MessagesFile: "compiler:Default.isl"

[Tasks]
Name: "desktopicon"; Description: "{cm:CreateDesktopIcon}"

[Icons]
Name: "{group}\{#MyAppName}"; Filename: "{app}\{#MyAppExeName}"; IconFilename: "{app}\default.ico"
Name: "{group}\{cm:UninstallProgram,{#MyAppName}}"; Filename: "{uninstallexe}"
Name: "{commonprograms}\{#MyAppName}"; Filename: "{app}\{#MyAppExeName}"; IconFilename: "{app}\default.ico"
Name: "{commondesktop}\{#MyAppName}"; Filename: "{app}\{#MyAppExeName}"; Tasks: desktopicon; IconFilename: "{app}\default.ico"

[Files]
Source: "LICENSE"; Flags: dontcopy
Source: "{#MyAppExeName}"; DestDir: "{app}"; Flags: ignoreversion
#if IncludeR
    Source: "R-{#RVersion}-win.exe"; DestDir: "{tmp}"; Check: RNeeded
#endif
#if IncludePandoc
    Source: "pandoc-{#PandocVersion}-windows.msi"; DestDir: "{tmp}"; Check: PandocNeeded
#endif
Source: "default.ico"; DestDir: "{app}"; Flags: ignoreversion;
Source: "IDBac.bat"; DestDir: "{app}"; Flags: ignoreversion;
Source: "LICENSE"; DestDir: "{app}"; Flags: ignoreversion;
Source: "server.r"; DestDir: "{app}"; Flags: ignoreversion;
Source: "setup.ico"; DestDir: "{app}"; Flags: ignoreversion;
Source: "SmallAppIcon.png"; DestDir: "{app}"; Flags: ignoreversion;
Source: "SmallSetupIcon.ico"; DestDir: "{app}"; Flags: ignoreversion;
Source: "ui.r"; DestDir: "{app}"; Flags: ignoreversion;
Source: "pwiz\agtsampleinforw.dll"; DestDir: "{app}\pwiz"; Flags: ignoreversion;
Source: "pwiz\baf2sql_c.dll"; DestDir: "{app}\pwiz"; Flags: ignoreversion;
Source: "pwiz\BaseCommon.dll"; DestDir: "{app}\pwiz"; Flags: ignoreversion;
Source: "pwiz\BaseDataAccess.dll"; DestDir: "{app}\pwiz"; Flags: ignoreversion;
Source: "pwiz\BaseDataAccess.dll.config"; DestDir: "{app}\pwiz"; Flags: ignoreversion;
Source: "pwiz\BaseError.dll"; DestDir: "{app}\pwiz"; Flags: ignoreversion;
Source: "pwiz\BaseTof.dll"; DestDir: "{app}\pwiz"; Flags: ignoreversion;
Source: "pwiz\BDal.BCO.Constants.xml"; DestDir: "{app}\pwiz"; Flags: ignoreversion;
Source: "pwiz\BDal.BCO.dll"; DestDir: "{app}\pwiz"; Flags: ignoreversion;
Source: "pwiz\BDal.BCO.Interfaces.dll"; DestDir: "{app}\pwiz"; Flags: ignoreversion;
Source: "pwiz\BDal.BCO.Objects.xml"; DestDir: "{app}\pwiz"; Flags: ignoreversion;
Source: "pwiz\BDal.CCO.Calibration.dll"; DestDir: "{app}\pwiz"; Flags: ignoreversion;
Source: "pwiz\BDal.CCO.Constants.xml"; DestDir: "{app}\pwiz"; Flags: ignoreversion;
Source: "pwiz\BDal.CCO.Interfaces.dll"; DestDir: "{app}\pwiz"; Flags: ignoreversion;
Source: "pwiz\BDal.CCO.Objects.xml"; DestDir: "{app}\pwiz"; Flags: ignoreversion;
Source: "pwiz\BDal.CCO.TemperatureCompensation.dll"; DestDir: "{app}\pwiz"; Flags: ignoreversion;
Source: "pwiz\BDal.CCO.Transformation.dll"; DestDir: "{app}\pwiz"; Flags: ignoreversion;
Source: "pwiz\BDal.CCO.Utilities.dll"; DestDir: "{app}\pwiz"; Flags: ignoreversion;
Source: "pwiz\BDal.CXt.Lc.dll"; DestDir: "{app}\pwiz"; Flags: ignoreversion;
Source: "pwiz\BDal.CXt.Lc.Factory.dll"; DestDir: "{app}\pwiz"; Flags: ignoreversion;
Source: "pwiz\BDal.CXt.Lc.Interfaces.dll"; DestDir: "{app}\pwiz"; Flags: ignoreversion;
Source: "pwiz\BDal.CXt.Lc.UntU2.dll"; DestDir: "{app}\pwiz"; Flags: ignoreversion;
Source: "pwiz\boost_date_time-vc90-mt-1_37-x64-BDAL_20091123.dll"; DestDir: "{app}\pwiz"; Flags: ignoreversion;
Source: "pwiz\boost_regex-vc90-mt-1_37-x64-BDAL_20091123.dll"; DestDir: "{app}\pwiz"; Flags: ignoreversion;
Source: "pwiz\boost_system-vc90-mt-1_37-x64-BDAL_20091123.dll"; DestDir: "{app}\pwiz"; Flags: ignoreversion;
Source: "pwiz\boost_thread-vc90-mt-1_37-x64-BDAL_20091123.dll"; DestDir: "{app}\pwiz"; Flags: ignoreversion;
Source: "pwiz\cdt.dll"; DestDir: "{app}\pwiz"; Flags: ignoreversion;
Source: "pwiz\Clearcore2.Compression.dll"; DestDir: "{app}\pwiz"; Flags: ignoreversion;
Source: "pwiz\Clearcore2.Data.Acquisition.Client.dll"; DestDir: "{app}\pwiz"; Flags: ignoreversion;
Source: "pwiz\Clearcore2.Data.Acquisition.Contracts.dll"; DestDir: "{app}\pwiz"; Flags: ignoreversion;
Source: "pwiz\Clearcore2.Data.AnalystDataProvider.dll"; DestDir: "{app}\pwiz"; Flags: ignoreversion;
Source: "pwiz\Clearcore2.Data.Client.dll"; DestDir: "{app}\pwiz"; Flags: ignoreversion;
Source: "pwiz\Clearcore2.Data.Common.dll"; DestDir: "{app}\pwiz"; Flags: ignoreversion;
Source: "pwiz\Clearcore2.Data.CommonInterfaces.dll"; DestDir: "{app}\pwiz"; Flags: ignoreversion;
Source: "pwiz\Clearcore2.Data.Contracts.dll"; DestDir: "{app}\pwiz"; Flags: ignoreversion;
Source: "pwiz\Clearcore2.Data.Core.dll"; DestDir: "{app}\pwiz"; Flags: ignoreversion;
Source: "pwiz\Clearcore2.Data.dll"; DestDir: "{app}\pwiz"; Flags: ignoreversion;
Source: "pwiz\Clearcore2.Data.Wiff2.dll"; DestDir: "{app}\pwiz"; Flags: ignoreversion;
Source: "pwiz\Clearcore2.Data.WiffReader.dll"; DestDir: "{app}\pwiz"; Flags: ignoreversion;
Source: "pwiz\Clearcore2.DataService.dll"; DestDir: "{app}\pwiz"; Flags: ignoreversion;
Source: "pwiz\Clearcore2.Devices.Types.dll"; DestDir: "{app}\pwiz"; Flags: ignoreversion;
Source: "pwiz\Clearcore2.Domain.Acquisition.dll"; DestDir: "{app}\pwiz"; Flags: ignoreversion;
Source: "pwiz\Clearcore2.Domain.Acquisition.Methods.MassSpec.dll"; DestDir: "{app}\pwiz"; Flags: ignoreversion;
Source: "pwiz\Clearcore2.Infrastructure.dll"; DestDir: "{app}\pwiz"; Flags: ignoreversion;
Source: "pwiz\Clearcore2.InternalRawXYProcessing.dll"; DestDir: "{app}\pwiz"; Flags: ignoreversion;
Source: "pwiz\Clearcore2.Muni.dll"; DestDir: "{app}\pwiz"; Flags: ignoreversion;
Source: "pwiz\Clearcore2.RawXYProcessing.dll"; DestDir: "{app}\pwiz"; Flags: ignoreversion;
Source: "pwiz\Clearcore2.StructuredStorage.dll"; DestDir: "{app}\pwiz"; Flags: ignoreversion;
Source: "pwiz\Clearcore2.UserLog.Types.dll"; DestDir: "{app}\pwiz"; Flags: ignoreversion;
Source: "pwiz\Clearcore2.Utility.dll"; DestDir: "{app}\pwiz"; Flags: ignoreversion;
Source: "pwiz\Clearcore2.XmlHelpers.dll"; DestDir: "{app}\pwiz"; Flags: ignoreversion;
Source: "pwiz\CompassXtractMS.dll"; DestDir: "{app}\pwiz"; Flags: ignoreversion;
Source: "pwiz\Compressor_12451AC8-3BF1-48e1-8E66-DA05BF3852A0.dll"; DestDir: "{app}\pwiz"; Flags: ignoreversion;
Source: "pwiz\Compressor_382D5134-9727-4be6-B6F8-754F577426D6.dll"; DestDir: "{app}\pwiz"; Flags: ignoreversion;
Source: "pwiz\Compressor_4F1EA0CB-5A6A-43ED-9562-D6D873034577.dll"; DestDir: "{app}\pwiz"; Flags: ignoreversion;
Source: "pwiz\Compressor_6BB2E64A-27A0-4575-A66A-4E312C8B9AD7.dll"; DestDir: "{app}\pwiz"; Flags: ignoreversion;
Source: "pwiz\Compressor_6EEE649E-09AA-4332-8E82-8188652E8AB5.dll"; DestDir: "{app}\pwiz"; Flags: ignoreversion;
Source: "pwiz\Compressor_7F88E97F-2038-40ba-8F3A-FCA9A9719569.dll"; DestDir: "{app}\pwiz"; Flags: ignoreversion;
Source: "pwiz\CustomDataSourceDialog.dll"; DestDir: "{app}\pwiz"; Flags: ignoreversion;
Source: "pwiz\CustomDataSourceDialog.dll.config"; DestDir: "{app}\pwiz"; Flags: ignoreversion;
Source: "pwiz\DataGridViewAutoFilter.dll"; DestDir: "{app}\pwiz"; Flags: ignoreversion;
Source: "pwiz\DataReader.dll"; DestDir: "{app}\pwiz"; Flags: ignoreversion;
Source: "pwiz\DigitalRune.Windows.Docking.dll"; DestDir: "{app}\pwiz"; Flags: ignoreversion;
Source: "pwiz\EULA.MHDAC"; DestDir: "{app}\pwiz"; Flags: ignoreversion;
Source: "pwiz\EULA.MSFileReader"; DestDir: "{app}\pwiz"; Flags: ignoreversion;
Source: "pwiz\EULA.SFCS"; DestDir: "{app}\pwiz"; Flags: ignoreversion;
Source: "pwiz\fileio_x64.dll"; DestDir: "{app}\pwiz"; Flags: ignoreversion;
Source: "pwiz\FlexVariableTable.xml"; DestDir: "{app}\pwiz"; Flags: ignoreversion;
Source: "pwiz\fregistry_x64.dll"; DestDir: "{app}\pwiz"; Flags: ignoreversion;
Source: "pwiz\HSReadWrite.dll"; DestDir: "{app}\pwiz"; Flags: ignoreversion;
Source: "pwiz\ICRVariableTable.xml"; DestDir: "{app}\pwiz"; Flags: ignoreversion;
Source: "pwiz\Interop.DataExplorer.dll"; DestDir: "{app}\pwiz"; Flags: ignoreversion;
Source: "pwiz\Interop.EDAL.dll"; DestDir: "{app}\pwiz"; Flags: ignoreversion;
Source: "pwiz\Interop.EDAL.SxS.manifest"; DestDir: "{app}\pwiz"; Flags: ignoreversion;
Source: "pwiz\Interop.HSREADWRITELib.dll"; DestDir: "{app}\pwiz"; Flags: ignoreversion;
Source: "pwiz\Interop.HSREADWRITELib.SxS.manifest"; DestDir: "{app}\pwiz"; Flags: ignoreversion;
Source: "pwiz\Interop.MSFileReaderLib.dll"; DestDir: "{app}\pwiz"; Flags: ignoreversion;
Source: "pwiz\MassLynxRaw.dll"; DestDir: "{app}\pwiz"; Flags: ignoreversion;
Source: "pwiz\MassSpecDataReader.dll"; DestDir: "{app}\pwiz"; Flags: ignoreversion;
Source: "pwiz\Microsoft.Practices.Unity.Configuration.dll"; DestDir: "{app}\pwiz"; Flags: ignoreversion;
Source: "pwiz\Microsoft.Practices.Unity.dll"; DestDir: "{app}\pwiz"; Flags: ignoreversion;
Source: "pwiz\MIDAC.dll"; DestDir: "{app}\pwiz"; Flags: ignoreversion;
Source: "pwiz\mkl_sequential.dll"; DestDir: "{app}\pwiz"; Flags: ignoreversion;
Source: "pwiz\msconvert.exe"; DestDir: "{app}\pwiz"; Flags: ignoreversion;
Source: "pwiz\MSConvertGUI.exe.manifest"; DestDir: "{app}\pwiz"; Flags: ignoreversion;
Source: "pwiz\MSFileReader.XRawfile2.dll"; DestDir: "{app}\pwiz"; Flags: ignoreversion;
Source: "pwiz\MSFileReader.XRawfile2.SxS.manifest"; DestDir: "{app}\pwiz"; Flags: ignoreversion;
Source: "pwiz\MSGraph.dll"; DestDir: "{app}\pwiz"; Flags: ignoreversion;
Source: "pwiz\msparser.dll"; DestDir: "{app}\pwiz"; Flags: ignoreversion;
Source: "pwiz\OFX.Core.Contracts.dll"; DestDir: "{app}\pwiz"; Flags: ignoreversion;
Source: "pwiz\OFX.Core.dll"; DestDir: "{app}\pwiz"; Flags: ignoreversion;
Source: "pwiz\OFX.EventServices.dll"; DestDir: "{app}\pwiz"; Flags: ignoreversion;
Source: "pwiz\OFX.Logging.dll"; DestDir: "{app}\pwiz"; Flags: ignoreversion;
Source: "pwiz\OFX.Security.Core.dll"; DestDir: "{app}\pwiz"; Flags: ignoreversion;
Source: "pwiz\protobuf-net.dll"; DestDir: "{app}\pwiz"; Flags: ignoreversion;
Source: "pwiz\pwiz.CommonUtil.dll"; DestDir: "{app}\pwiz"; Flags: ignoreversion;
Source: "pwiz\pwiz_bindings_cli.dll"; DestDir: "{app}\pwiz"; Flags: ignoreversion;
Source: "pwiz\README.md"; DestDir: "{app}\pwiz"; Flags: ignoreversion;
Source: "pwiz\Sciex.Clearcore.FMan.dll"; DestDir: "{app}\pwiz"; Flags: ignoreversion;
Source: "pwiz\Sciex.Data.Processing.dll"; DestDir: "{app}\pwiz"; Flags: ignoreversion;
Source: "pwiz\Sciex.Data.SimpleTypes.dll"; DestDir: "{app}\pwiz"; Flags: ignoreversion;
Source: "pwiz\Sciex.Data.XYData.dll"; DestDir: "{app}\pwiz"; Flags: ignoreversion;
Source: "pwiz\Sciex.FMan.dll"; DestDir: "{app}\pwiz"; Flags: ignoreversion;
Source: "pwiz\Sciex.Wiff.dll"; DestDir: "{app}\pwiz"; Flags: ignoreversion;
Source: "pwiz\seems.exe.config"; DestDir: "{app}\pwiz"; Flags: ignoreversion;
Source: "pwiz\seems.exe.manifest"; DestDir: "{app}\pwiz"; Flags: ignoreversion;
Source: "pwiz\SQLite_v1.0.98\System.Data.SQLite.dll"; DestDir: "{app}\pwiz\SQLite_v1098"; Flags: ignoreversion;
Source: "pwiz\STL_Containers.dll"; DestDir: "{app}\pwiz"; Flags: ignoreversion;
Source: "pwiz\System.Data.SQLite.dll"; DestDir: "{app}\pwiz"; Flags: ignoreversion;
Source: "pwiz\UIMFLibrary.dll"; DestDir: "{app}\pwiz"; Flags: ignoreversion;
Source: "pwiz\unimod_2.xsd"; DestDir: "{app}\pwiz"; Flags: ignoreversion;
Source: "pwiz\ZedGraph.dll"; DestDir: "{app}\pwiz"; Flags: ignoreversion;
Source: "utils\app.R"; DestDir: "{app}\utils"; Flags: ignoreversion;
Source: "utils\config.cfg"; DestDir: "{app}\utils"; Flags: ignoreversion;
Source: "utils\ensure.R"; DestDir: "{app}\utils"; Flags: ignoreversion;
Source: "utils\get_app_from_app_url.R"; DestDir: "{app}\utils"; Flags: ignoreversion;
Source: "utils\package_manager.R"; DestDir: "{app}\utils"; Flags: ignoreversion;
Source: "utils\wsf\js\JSON.minify.js"; DestDir: "{app}\utils\wsf\js"; Flags: ignoreversion;
Source: "utils\wsf\js\json2.js"; DestDir: "{app}\utils\wsf\js"; Flags: ignoreversion;
Source: "utils\wsf\js\run.js"; DestDir: "{app}\utils\wsf\js"; Flags: ignoreversion;
Source: "utils\wsf\run.wsf"; DestDir: "{app}\utils\wsf"; Flags: ignoreversion;
Source: "www\Multi-MALDI-Plate.png"; DestDir: "{app}\www"; Flags: ignoreversion;
Source: "www\placeholder.gif"; DestDir: "{app}\www"; Flags: ignoreversion;
Source: "www\window.PNG"; DestDir: "{app}\www"; Flags: ignoreversion;
Source: "www\WorkingDirectory.png"; DestDir: "{app}\www"; Flags: ignoreversion;

[Run]
#if IncludeR
	Filename: "{tmp}\R-{#RVersion}-win.exe"; Parameters: "/SILENT"; WorkingDir: {tmp}; Flags: skipifdoesntexist; StatusMsg: "Installing R if needed"
#endif
#if IncludePandoc
	Filename: "msiexec.exe"; Parameters: "/i ""{tmp}\pandoc-{#PandocVersion}-windows.msi"" /q"; WorkingDir: {tmp}; Flags: skipifdoesntexist; StatusMsg: "Installing Pandoc if needed"
#endif
Filename: "{app}\{#MyAppExeName}"; Description: "{cm:LaunchProgram,{#StringChange(MyAppName, '&', '&&')}}"; Flags: shellexec postinstall skipifsilent


[UninstallDelete]
Type: filesandordirs; Name: "{app}\library";
Type: filesandordirs; Name: "{app}\utils";
Type: filesandordirs; Name: "{app}\log";

[Code]
const
  RRegKey = 'Software\R-Core\R\{#RVersion}';
  ChromeRegKey = 'Software\Microsoft\Windows\CurrentVersion\App Paths\chrome.exe';
  IERegKey = 'Software\Microsoft\Windows\CurrentVersion\App Paths\IEXPLORE.EXE';
  FFRegKey = 'Software\Microsoft\Windows\CurrentVersion\App Paths\firefox.exe';
var
  RegPathsFile: string;
  SecondLicensePage: TOutputMsgMemoWizardPage;
  License2AcceptedRadio: TRadioButton;
  License2NotAcceptedRadio: TRadioButton;

// Is R installed?
function RDetected(): boolean;
var
    success: boolean;
begin
  success := RegKeyExists(HKLM, RRegKey) or RegKeyExists(HKCU, RRegKey);
  begin
    Result := success;
  end;
end;

// If R is not detected, it is needed
function RNeeded(): Boolean;
begin
  Result := (RDetected = false);
end;

// Registry path update function (adds an extra backslash for json)
function AddBackSlash(Value: string): string;
begin
  Result := Value;
  StringChangeEx(Result, '\', '\\', True);
end;

// Pandoc is stored in the System PATH
function PandocDetected(): Boolean;
var
  PandocDir, Path: String;
begin
  Log('Checking for Pandoc in %PATH%');
  if RegQueryStringValue(HKEY_CURRENT_USER, 'Environment', 'Path', Path) then
  begin // Successfully read the value
    Log('HKCU\Environment\PATH = ' + Path);
    PandocDir := ExpandConstant('{localappdata}\Pandoc\');
    Log('Looking for Pandoc in %PATH%: ' + PandocDir + ' in ' + Path);
    if Pos(LowerCase(PandocDir), Lowercase(Path)) = 0 then
		begin
			Log('Did not find Pandoc in %PATH%');
			Result := False;
		end
    else
		begin
			Log('Found Pandoc in %PATH%');
			Result := True;
		end
  end
  else // The key probably doesn't exist
  begin
    Log('Could not access HKCU\Environment\PATH.');
    Result := False;
  end;
end;

// If Pandoc is not detected, it is needed
function PandocNeeded(): Boolean;
begin
  Result := (PandocDetected = false);
end;

// Save installation paths
procedure CurStepChanged(CurStep: TSetupStep);
var
  RPath, ChromePath, IEPath, FFPath, PandocPath: string;
begin
if CurStep = ssPostInstall then begin
    RPath := '';
    ChromePath := '';
    IEPath := '';
    FFPath := '';
		PandocPath := ExpandConstant('{localappdata}\Pandoc\');
    RegPathsFile := ExpandConstant('{app}\utils\regpaths.json');
    // Create registry paths file
    SaveStringToFile(RegPathsFile, '{' + #13#10, True);

    // R RegPath
    if RegQueryStringValue(HKLM, RRegKey, 'InstallPath', RPath) or RegQueryStringValue(HKCU, RRegKey, 'InstallPath', RPath) then
      SaveStringToFile(RegPathsFile, '"r": "' + AddBackSlash(RPath) + '",' + #13#10, True)
    else
      SaveStringToFile(RegPathsFile, '"r": "none",' + #13#10, True);

    // Chrome RegPath
    if RegQueryStringValue(HKLM, ChromeRegKey, 'Path', ChromePath) then
      SaveStringToFile(RegPathsFile, '"chrome": "' + AddBackSlash(ChromePath) + '",' + #13#10, True)
    else
      SaveStringToFile(RegPathsFile, '"chrome": "none",' + #13#10, True);

    // Internet Explorer RegPath
    if RegQueryStringValue(HKLM, IERegKey, '', IEPath) then
      SaveStringToFile(RegPathsFile, '"ie": "' + AddBackSlash(IEPath) + '",' + #13#10, True)
    else
      SaveStringToFile(RegPathsFile, '"ie": "none",' + #13#10, True);

    // Firefox RegPath
    if RegQueryStringValue(HKLM, FFRegKey, 'Path', FFPath) then
      SaveStringToFile(RegPathsFile, '"ff": "' + AddBackSlash(FFPath) + '",' + #13#10, True)
    else
      SaveStringToFile(RegPathsFile, '"ff": "none",' + #13#10, True);

		// Pandoc RegPath
		// ** Last Line in json file (no trailing comma) **
		if PandocDetected() then
			SaveStringToFile(RegPathsFile, '"pandoc": "' + AddBackSlash(PandocPath) + '"' + #13#10, True)
		else
			SaveStringToFile(RegPathsFile, '"pandoc": "none"' + #13#10, True);

    SaveStringToFile(RegPathsFile, '}', True);
  end;
end;

// Add RInno's license to the installer
procedure CheckLicense2Accepted(Sender: TObject);
begin
  { Update Next button when user (un)accepts the license }
  WizardForm.NextButton.Enabled := License2AcceptedRadio.Checked;
end;

function CloneLicenseRadioButton(Source: TRadioButton): TRadioButton;
begin
  Result := TRadioButton.Create(WizardForm);
  Result.Parent := SecondLicensePage.Surface;
  Result.Caption := Source.Caption;
  Result.Left := Source.Left;
  Result.Top := Source.Top;
  Result.Width := Source.Width;
  Result.Height := Source.Height;
  Result.OnClick := @CheckLicense2Accepted;
end;

procedure InitializeWizard();
var
  LicenseFileName: string;
  LicenseFilePath: string;
begin
  { Create second license page, with the same labels as the original license page }
  SecondLicensePage :=
    CreateOutputMsgMemoPage(
      wpLicense, SetupMessage(msgWizardLicense), SetupMessage(msgLicenseLabel),
      SetupMessage(msgLicenseLabel3), '');

  { Shrink license box to make space for radio buttons }
  SecondLicensePage.RichEditViewer.Height := WizardForm.LicenseMemo.Height;

  { Load license }
  { Loading ex-post, as Lines.LoadFromFile supports UTF-8, }
  { contrary to LoadStringFromFile. }
  LicenseFileName := 'LICENSE';
  ExtractTemporaryFile(LicenseFileName);
  LicenseFilePath := ExpandConstant('{tmp}\' + LicenseFileName);
  SecondLicensePage.RichEditViewer.Lines.LoadFromFile(LicenseFilePath);
  DeleteFile(LicenseFilePath);

  { Clone accept/do not accept radio buttons for the second license }
  License2AcceptedRadio := CloneLicenseRadioButton(WizardForm.LicenseAcceptedRadio);
  License2NotAcceptedRadio := CloneLicenseRadioButton(WizardForm.LicenseNotAcceptedRadio);

  { Initially not accepted }
  License2NotAcceptedRadio.Checked := True;
end;

procedure CurPageChanged(CurPageID: Integer);
begin
  { Update Next button when user gets to second license page }
  if CurPageID = SecondLicensePage.ID then
  begin
    CheckLicense2Accepted(nil);
  end;
end;
