{ *********************************************************** }
{ * Unit with Windows Media Player interfaces for ActiveX   * }
{ * Rewritten in Pascal from C/C++ wmp.h from Windows SDK   * }
{ * Modera, 2023                                            * }
{ *********************************************************** }
unit WindowsMediaPlayer;

interface

type
	VARIANT_BOOL = Smallint;
	PVARIANT_BOOL = ^VARIANT_BOOL;
	
type
	// size of these ?
	WMPOpenState = (
		wmposUndefined,
		wmposPlaylistChanging,
		wmposPlaylistLocating,
		wmposPlaylistConnecting,
		wmposPlaylistLoading,
		wmposPlaylistOpening,
		wmposPlaylistOpenNoMedia,
		wmposPlaylistChanged,
		wmposMediaChanging,
		wmposMediaLocating,
		wmposMediaConnecting,
		wmposMediaLoading,
		wmposMediaOpening,
		wmposMediaOpen,
		wmposBeginCodecAcquisition,
		wmposEndCodecAcquisition,
		wmposBeginLicenseAcquisition,
		wmposEndLicenseAcquisition,
		wmposBeginIndividualization,
		wmposEndIndividualization,
		wmposMediaWaiting,
		wmposOpeningUnknownURL
	);
	PWMPOpenState = ^WMPOpenState;
	
	WMPPlayState = (
		wmppsUndefined,
		wmppsStopped,
		wmppsPaused,
		wmppsPlaying,
		wmppsScanForward,
		wmppsScanReverse,
		wmppsBuffering,
		wmppsWaiting,
		wmppsMediaEnded,
		wmppsTransitioning,
		wmppsReady,
		wmppsReconnecting,
		wmppsLast 
	);
	PWMPPlayState = ^WMPPlayState;

	WMPPlaylistChangeEventType = (
		wmplcUnknown,
		wmplcClear,
		wmplcInfoChange,
		wmplcMove,
		wmplcDelete,
		wmplcInsert,
		wmplcAppend,
		wmplcPrivate,
		wmplcNameChange,
		wmplcMorph,
		wmplcSort,
		wmplcLast 
	);
	PWMPPlaylistChangeEventType = ^WMPPlaylistChangeEventType;
	
	WMPSyncState = (
		wmpssUnknown,
		wmpssSynchronizing,
		wmpssStopped,
		wmpssLast
	);
	PWMPSyncState = ^WMPSyncState;

	WMPDeviceStatus = (
		wmpdsUnknown,
		wmpdsPartnershipExists ,
		wmpdsPartnershipDeclined,
		wmpdsPartnershipAnother,
		wmpdsManualDevice,
		wmpdsNewDevice,
		wmpdsLast
	);
	PWMPDeviceStatus = ^WMPDeviceStatus;
	
	WMPRipState = (
		wmprsUnknown,
		wmprsRipping,
		wmprsStopped
	);
	PWMPRipState = ^WMPRipState;

	WMPBurnFormat = (
		wmpbfAudioCD,
		wmpbfDataCD 
	);
	PWMPBurnFormat = ^WMPBurnFormat;

	WMPBurnState = (
		wmpbsUnknown,
		wmpbsBusy,
		wmpbsReady,
		wmpbsWaitingForDisc,
		wmpbsRefreshStatusPending,
		wmpbsPreparingToBurn,
		wmpbsBurning,
		wmpbsStopped,
		wmpbsErasing,
		wmpbsDownloading
	);
	PWMPBurnState = ^WMPBurnState;

	WMPStringCollectionChangeEventType = (
		wmpsccetUnknown,
		wmpsccetInsert,
		wmpsccetChange,
		wmpsccetDelete,
		wmpsccetClear,
		wmpsccetBeginUpdates,
		wmpsccetEndUpdates
	);
	PWMPStringCollectionChangeEventType = ^WMPStringCollectionChangeEventType;
	
	WMPLibraryType = (
		wmpltUnknown,
		wmpltAll,
		wmpltLocal,
		wmpltRemote,
		wmpltDisc,
		wmpltPortableDevice
	);
	PWMPLibraryType = ^WMPLibraryType;
	
	WMPFolderScanState = (
		wmpfssUnknown,
		wmpfssScanning,
		wmpfssUpdating,
		wmpfssStopped
	);
	PWMPFolderScanState = ^WMPFolderScanState;

type
	IWMPPlaylist = interface;
	
	IWMPErrorItem = interface(IDispatch)
		['{3614C646-3B3B-4de7-A81E-930E3F2127B3}']
		function get_errorCode(phr : PLongint) : HRESULT; stdcall;
		function get_errorDescription(out pbstrDescription : WideString) : HRESULT; stdcall;
		function get_errorContext(pvarContext : POleVariant) : HRESULT; stdcall;
		function get_remedy(plRemedy : PLongint) : HRESULT; stdcall;
		function get_customUrl(out pbstrCustomUrl : WideString) : HRESULT; stdcall;
	end;
	
	IWMPError = interface(IDispatch)
		['{A12DCF7D-14AB-4c1b-A8CD-63909F06025B}']
		function clearErrorQueue : HRESULT; stdcall;
		function get_errorCount(plNumErrors : PLongint) : HRESULT; stdcall;
		function get_item(dwIndex : Longint; out ppErrorItem : IWMPErrorItem) : HRESULT; stdcall;
		function webHelp : HRESULT; stdcall;
	end;
	
	IWMPMedia = interface(IDispatch)
		['{94D55E95-3FAC-11d3-B155-00C04F79FAA6}']
		function get_isIdentical(pIWMPMedia : IWMPMedia; pvbool : PVARIANT_BOOL) : HRESULT; stdcall;
		function get_sourceURL(out pbstrSourceURL : WideString) : HRESULT; stdcall;
		function get_name(out pbstrName : WideString) : HRESULT; stdcall;
		function put_name(bstrName : WideString) : HRESULT; stdcall;
		function get_imageSourceWidth(pWidth : PLongint) : HRESULT; stdcall;
		function get_imageSourceHeight(pHeight : PLongint) : HRESULT; stdcall;
		function get_markerCount(pMarkerCount : PLongint) : HRESULT; stdcall;
		function getMarkerTime(MarkerNum : Longint; pMarkerTime : PDouble) : HRESULT; stdcall;
		function getMarkerName(MarkerNum : Longint; out pbstrMarkerName : WideString) : HRESULT; stdcall;
		function get_duration(pDuration : PDouble) : HRESULT; stdcall;
		function get_durationString(out pbstrDuration : WideString) : HRESULT; stdcall;
		function get_attributeCount(plCount : PLongint) : HRESULT; stdcall;
		function getAttributeName(lIndex : Longint; out pbstrItemName : WideString) : HRESULT; stdcall;
		function getItemInfo(bstrItemName : WideString; out pbstrVal : WideString) : HRESULT; stdcall;
		function setItemInfo(bstrItemName : WideString; bstrVal : WideString) : HRESULT; stdcall;
		function getItemInfoByAtom(lAtom : Longint; out pbstrVal : WideString) : HRESULT; stdcall;
		function isMemberOf(pPlayList : IWMPPlaylist; pvarfIsMemberOf : PVARIANT_BOOL) : HRESULT; stdcall;
		function isReadOnlyItem(bstrItemName : WideString; pvarfIsReadOnly : PVARIANT_BOOL) : HRESULT; stdcall;
	end;
	
	IWMPControls = interface(IDispatch)
		['{74C09E02-F828-11d2-A74B-00A0C905F36E}']
		function get_isAvailable(bstrItem : WideString; pIsAvailable : PVARIANT_BOOL) : HRESULT; stdcall;
		function play : HRESULT; stdcall;
		function stop : HRESULT; stdcall;
		function pause : HRESULT; stdcall;
		function fastForward : HRESULT; stdcall;
		function fastReverse : HRESULT; stdcall;
		function get_currentPosition(pdCurrentPosition : PDouble) : HRESULT; stdcall;
		function put_currentPosition(dCurrentPosition : Double) : HRESULT; stdcall;
		function get_currentPositionString(out pbstrCurrentPosition : WideString) : HRESULT; stdcall;
		function next : HRESULT; stdcall;
		function prevous : HRESULT; stdcall;
		function get_currentItem(out ppIWMPMedia : IWMPMedia) : HRESULT; stdcall;
		function put_currentItem(pIWMPMedia : IWMPMedia) : HRESULT; stdcall;
		function get_currentMarker(plMarker : PLongint) : HRESULT; stdcall;
		function put_currentMarker(lMarker : Longint) : HRESULT; stdcall;
		function playItem(pIWMPMedia : IWMPMedia) : HRESULT; stdcall;
	end;
	
	IWMPSettings = interface(IDispatch)
		['{9104D1AB-80C9-4fed-ABF0-2E6417A6DF14}']
		function get_isAvailable(bstrItem : WideString; pIsAvailable : PVARIANT_BOOL) : HRESULT; stdcall;
		function get_autoStart(pfAutoStart : PVARIANT_BOOL) : HRESULT; stdcall;
		function put_autoStart(fAutoStart : VARIANT_BOOL) : HRESULT; stdcall;
		function get_baseURL(out pbstrBaseURL : WideString) : HRESULT; stdcall;
		function put_baseURL(bstrBaseURL : WideString) : HRESULT; stdcall;
		function get_defaultFrame(out pbstrDefaultFrame : WideString) : HRESULT; stdcall;
		function put_defaultFrame(bstrDefaultFrame : WideString) : HRESULT; stdcall;
		function get_invokeURLs(pfInvokeURLs : PVARIANT_BOOL) : HRESULT; stdcall;
		function put_invokeURLs(fInvokeURLs : VARIANT_BOOL) : HRESULT; stdcall;
		function get_mute(pfMute : PVARIANT_BOOL) : HRESULT; stdcall;
		function put_mute(fMute : VARIANT_BOOL) : HRESULT; stdcall;		
		function get_playCount(plCount : PLongint) : HRESULT; stdcall;
		function put_playCount(lCount : Longint) : HRESULT; stdcall;
		function get_rate(pdRate : PDouble) : HRESULT; stdcall;
		function put_rate(dRate : Double) : HRESULT; stdcall;
		function get_balance(plBalance : PLongint) : HRESULT; stdcall;
		function put_balance(lBalance : Longint) : HRESULT; stdcall;
		function get_volume(plVolume : PLongint) : HRESULT; stdcall;
		function put_volume(lVolume : Longint) : HRESULT; stdcall;
		function getMode(bstrMode : WideString; pvarfMode : PVARIANT_BOOL) : HRESULT; stdcall;
		function setMode(bstrMode : WideString; varfMode : VARIANT_BOOL) : HRESULT; stdcall;
		function get_enableErrorDialogs(pfEnableErrorDialogs : PVARIANT_BOOL) : HRESULT; stdcall;
		function set_enableErrorDialogs(fEnableErrorDialogs : VARIANT_BOOL) : HRESULT; stdcall;
	end;
	
	IWMPClosedCaption = interface(IDispatch)
		['{4F2DF574-C588-11d3-9ED0-00C04FB6E937}']
		function get_SAMIStyle(out pbstrSAMIStyle : WideString) : HRESULT; stdcall;
		function put_SAMIStyle(bstrSAMIStyle : WideString) : HRESULT; stdcall;
		function get_SAMILang(out pbstrSAMILang : WideString) : HRESULT; stdcall;
		function put_SAMILang(bstrSAMILang : WideString) : HRESULT; stdcall;
		function get_SAMIFileName(out pbstrSAMIFileName : WideString) : HRESULT; stdcall;
		function put_SAMIFileName(bstrSAMIFileName : WideString) : HRESULT; stdcall;
		function get_captioningId(out pbstrCaptioningID : WideString) : HRESULT; stdcall;
		function put_captioningId(bstrCaptioningID : WideString) : HRESULT; stdcall;
	end;
	
	IWMPPlaylist = interface(IDispatch)
		['{D5F0F4F1-130C-11d3-B14E-00C04F79FAA6}']
		function get_count(plCount : PLongint) : HRESULT; stdcall;
		function get_name(out pbstrName : WideString) : HRESULT; stdcall;
		function put_name(bstrName : WideString) : HRESULT; stdcall;
		function get_attributeCount(plCount : PLongint) : HRESULT; stdcall;
		function get_attributeName(lIndex : Longint; out pbstrAttributeName : WideString) : HRESULT; stdcall;
		function get_item(lIndex : Longint; out ppIWMPMedia : IWMPMedia) : HRESULT; stdcall;
		function getItemInfo(bstrName : WideString; out pbstrVal : WideString) : HRESULT; stdcall;
		function setItemInfo(bstrName : WideString; bstrVal : WideString) : HRESULT; stdcall;
		function get_isIdentical(pIWMPPlaylist : IWMPPlaylist; pvbool : PVARIANT_BOOL) : HRESULT; stdcall;
		function clear : HRESULT; stdcall;
		function insertItem(lIndex : Longint; pIWMPMedia : IWMPMedia) : HRESULT; stdcall;
		function appendItem(pIWMPMedia : IWMPMedia) : HRESULT; stdcall;
		function removeItem(pIWMPMedia : IWMPMedia) : HRESULT; stdcall;
		function moveItem(lIndexOld, lIndexNew : Longint) : HRESULT; stdcall;
	end;
	
	IWMPCdrom = interface(IDispatch)
		['{cfab6e98-8730-11d3-b388-00c04f68574b}']
		function get_driveSpecifier(out pbstrDrive : WideString) : HRESULT; stdcall;
		function get_playlist(out ppPlaylist : IWMPPlaylist) : HRESULT; stdcall;
		function eject : HRESULT; stdcall;
	end;
	
	IWMPCdromCollection = interface(IDispatch)
		['{EE4C8FE2-34B2-11d3-A3BF-006097C9B344}']
		function get_count(plCount : PLongint) : HRESULT; stdcall;
		function item(lIndex : Longint; out ppItem : IWMPCdrom) : HRESULT; stdcall;
		function getByDriveSpecifier(bstrDriveSpecifier : WideString; out ppCdrom : IWMPCdrom) : HRESULT; stdcall;
	end;
	
	IWMPStringCollection = interface(IDispatch)
		['{4a976298-8c0d-11d3-b389-00c04f68574b}']
		function get_count(plCount : PLongint) : HRESULT; stdcall;
		function item(lIndex : Longint; out pbstrString : WideString) : HRESULT; stdcall;
	end;
	
	IWMPMediaCollection = interface(IDispatch)
		['{8363BC22-B4B4-4b19-989D-1CD765749DD1}']
		function add(bstrURL : WideString; out ppItem : IWMPMedia) : HRESULT; stdcall;
		function getAll(out ppMediaItems : IWMPPlaylist) : HRESULT; stdcall;
		function getByName(bstrName : WideString; out ppMediaItems : IWMPPlayList) : HRESULT; stdcall;
		function getByGenre(bstrGenre : WideString; out ppMediaItems : IWMPPlayList) : HRESULT; stdcall;
		function getByAuthor(bstrAuthor : WideString; out ppMediaItems : IWMPPlayList) : HRESULT; stdcall;
		function getByAlbum(bstrAlbum : WideString; out ppMediaItems : IWMPPlayList) : HRESULT; stdcall;
		function getByAttribute(bstrAttribute, bstrValue : WideString; out ppMediaItems : IWMPPlayList) : HRESULT; stdcall;
		function remove(pItem : IWMPMedia; varfDeleteFile : VARIANT_BOOL) : HRESULT; stdcall;
		function getAttributeStringCollection(bstrAttribute, bstrMediaType : WideString; out ppStringCollection : IWMPStringCollection) : HRESULT; stdcall;
		function getMediaAtom(bstrItemName : WideString; plAtom : PLongint) : HRESULT; stdcall;
		function setDeleted(pItem : IWMPMedia; varfIsDeleted : VARIANT_BOOL) : HRESULT; stdcall;
		function isDeleted(pItem : IWMPMedia; pvarfIsDeleted : PVARIANT_BOOL) : HRESULT; stdcall;
	end;
	
	IWMPPlaylistArray = interface(IDispatch)
		['{679409c0-99f7-11d3-9fb7-00105aa620bb}']
		function get_count(plCount : PLongint) : HRESULT; stdcall;
		function item(lIndex : Longint; out ppItem : IWMPPlaylist) : HRESULT; stdcall;		
	end;
	
	IWMPPlaylistCollection = interface(IDispatch)
		['{10A13217-23A7-439b-B1C0-D847C79B7774}']
		function newPlayList(bstrName : WideString; out ppItem : IWMPPlaylist) : HRESULT; stdcall;
		function getAll(out ppPlaylistArray : IWMPPlaylistArray) : HRESULT; stdcall;
		function getByName(bstrName : WideString; out ppPlaylistArray : IWMPPlaylistArray) : HRESULT; stdcall;
		function remove(pItem : IWMPPlaylist) : HRESULT; stdcall;
		function setDeleted(pItem : IWMPPlaylist; varfIsDeleted : VARIANT_BOOL) : HRESULT; stdcall;
		function isDeleted(pItem : IWMPPlaylist; pvarfIsDeleted : PVARIANT_BOOL) : HRESULT; stdcall;
		function importPlaylist(pItem : IWMPPlaylist; out ppImportedItem : IWMPPlaylist) : HRESULT; stdcall;
	end;
	
	IWMPNetwork = interface(IDispatch)
		['{EC21B779-EDEF-462d-BBA4-AD9DDE2B29A7}']
		function get_bandWidth(plBandwidth : PLongint) : HRESULT; stdcall;
		function get_recoveredPackets(plRecoveredPackets : PLongint) : HRESULT; stdcall;
		function get_sourceProtocol(out pbstrSourceProtocol : WideString) : HRESULT; stdcall;
		function get_receivedPackets(plReceivedPackets : PLongint) : HRESULT; stdcall;
		function get_lostPackets(plLostPackets : PLongint) : HRESULT; stdcall;
		function get_receptionQuality(plReceptionQuality : PLongint) : HRESULT; stdcall;
		function get_bufferingCount(plBufferingCount : PLongint) : HRESULT; stdcall;
		function get_bufferingProgress(plBufferingProgress : PLongint) : HRESULT; stdcall;
		function get_bufferingTime(plBufferingTime : PLongint) : HRESULT; stdcall;
		function put_bufferingTime(lBufferingTime : Longint) : HRESULT; stdcall;
		function get_frameRate(plFrameRate : PLongint) : HRESULT; stdcall;
		function get_maxBitRate(plBitRate : PLongint) : HRESULT; stdcall;
		function get_bitRate(plBitRate : PLongint) : HRESULT; stdcall;
		function getProxySettings(bstrProtocol : WideString; plProxySetting : PLongint) : HRESULT; stdcall;
		function setProxySettings(bstrProtocol : WideString; lProxySetting : Longint) : HRESULT; stdcall;
		function getProxyName(bstrProtocol : WideString; out pbstrProxyName : WideString) : HRESULT; stdcall;
		function setProxyName(bstrProtocol, bstrProxyName : WideString) : HRESULT; stdcall;
		function getProxyPort(bstrProtocol : WideString; lProxyPort : PLongint) : HRESULT; stdcall;
		function setProxyPort(bstrProtocol : WideString; lProxyPort : Longint) : HRESULT; stdcall;
		function getProxyExceptionList(bstrProtocol : WideString; out pbstrExceptionList : WideString) : HRESULT; stdcall;
		function setProxyExceptionList(bstrProtocol, pbstrExceptionList : WideString) : HRESULT; stdcall;
		function getProxyBypassForLocal(bstrProtocol : WideString; pfBypassForLocal : PVARIANT_BOOL) : HRESULT; stdcall;
		function setProxyBypassForLocal(bstrProtocol : WideString; fBypassForLocal : VARIANT_BOOL) : HRESULT; stdcall;
		function get_maxBandwidth(lMaxBandwidth : PLongint) : HRESULT; stdcall;
		function put_maxBandwidth(lMaxBandwidth : Longint) : HRESULT; stdcall;
		function get_downloadProgress(plDownloadProgress : PLongint) : HRESULT; stdcall;
		function get_encodedFrameRate(plFrameRate : PLongint) : HRESULT; stdcall;
		function get_framesSkipped(plFrames : PLongint) : HRESULT; stdcall;
	end;
	
	IWMPCore = interface(IDispatch)
		['{D84CCA99-CCE2-11d2-9ECC-0000F8085981}']
		function close : HRESULT; stdcall;
		function get_URL(out pbstrURL : WideString) : HRESULT; stdcall;
		function put_URL(bstrURL : WideString) : HRESULT; stdcall;
		function get_openState(pwmpos : PWMPOpenState) : HRESULT; stdcall;
		function get_playState(pwmpps : PWMPPlayState) : HRESULT; stdcall;
		function get_controls(out ppControl : IWMPControls) : HRESULT; stdcall;
		function get_settings(out ppSettings : IWMPSettings) : HRESULT; stdcall;
		function get_currentMedia(out ppMedia : IWMPMedia) : HRESULT; stdcall;
		function put_currentMedia(pMedia : IWMPMedia) : HRESULT; stdcall;
		function get_mediaCollection(out ppMediaColletion : IWMPMediaCollection) : HRESULT; stdcall;
		function get_playlistColection(out ppPlaylistCollection : IWMPPlaylistCollection) : HRESULT; stdcall;
		function get_versionInfo(out pbstrVersionInfo : WideString) : HRESULT; stdcall;
		function launchURL(bstr : WideString) : HRESULT; stdcall;
		function get_network(out ppQNI : IWMPNetwork) : HRESULT; stdcall;
		function get_currentPlaylist(out ppPL : IWMPPlaylist) : HRESULT; stdcall;
		function put_currentPlaylist(pPL : IWMPPlaylist) : HRESULT; stdcall;
		function get_cdromCollection(out ppCdromCollection : IWMPCdromCollection) : HRESULT; stdcall;
		function get_closedCaption(out ppClosedCaption : IWMPClosedCaption) : HRESULT; stdcall;
		function get_isOnline(pfOnline : PVARIANT_BOOL) : HRESULT; stdcall;
		function get_error(out ppError : IWMPError) : HRESULT; stdcall;
		function get_status(out pbstrStatus : WideString) : HRESULT; stdcall;
	end;
	
	IWMPPlayer = interface(IWMPCore)
		['{6BF52A4F-394A-11d3-B153-00C04F79FAA6}']
		function get_enabled(pbEnabled : PVARIANT_BOOL) : HRESULT; stdcall;
		function put_enabled(bEnabled : VARIANT_BOOL) : HRESULT; stdcall;
		function get_fullScreen(pbFullScreen : PVARIANT_BOOL) : HRESULT; stdcall;
		function put_fullScreen(bFullScreen : VARIANT_BOOL) : HRESULT; stdcall;
		function get_enableContextMenu(pbEnableContextMenu : PVARIANT_BOOL) : HRESULT; stdcall;
		function put_enableContextMenu(bEnableContextMenu : VARIANT_BOOL) : HRESULT; stdcall;
		function put_uiMode(bstrMode : WideString) : HRESULT; stdcall;
		function get_uiMode(out pbstrMode : WideString) : HRESULT; stdcall;
	end;
	
	IWMPPlayer2 = interface(IWMPCore)
		['{0E6B01D1-D407-4c85-BF5F-1C01F6150280}']
		function get_enabled(pbEnabled : PVARIANT_BOOL) : HRESULT; stdcall;
		function put_enabled(bEnabled : VARIANT_BOOL) : HRESULT; stdcall;
		function get_fullScreen(pbFullScreen : PVARIANT_BOOL) : HRESULT; stdcall;
		function put_fullScreen(bFullScreen : VARIANT_BOOL) : HRESULT; stdcall;
		function get_enableContextMenu(pbEnableContextMenu : PVARIANT_BOOL) : HRESULT; stdcall;
		function put_enableContextMenu(bEnableContextMenu : VARIANT_BOOL) : HRESULT; stdcall;
		function put_uiMode(bstrMode : WideString) : HRESULT; stdcall;
		function get_uiMode(out pbstrMode : WideString) : HRESULT; stdcall;
		function get_stretchToFit(pbEnabled : PVARIANT_BOOL) : HRESULT; stdcall;
		function put_stretchToFit(bEnabled : VARIANT_BOOL) : HRESULT; stdcall;
		function get_windowlessVideo(pbEnabled : PVARIANT_BOOL) : HRESULT; stdcall;
		function put_windowlessVideo(bEnabled : VARIANT_BOOL) : HRESULT; stdcall;
	end;
	
	IWMPMedia2 = interface(IWMPMedia)
		['{AB7C88BB-143E-4ea4-ACC3-E4350B2106C3}']
		function get_error(out ppIWMpErrorItem : IWMPErrorItem) : HRESULT; stdcall;
	end;
	
	IWMPControls2 = interface(IWMPControls)
		['{6F030D25-0890-480f-9775-1F7E40AB5B8E}']
		function step(lStep : Longint) : HRESULT; stdcall;
	end;
	
	IWMPDVD = interface(IDispatch)
		['{8DA61686-4668-4a5c-AE5D-803193293DBE}']
		function get_isAvailable(bstrItem : WideString; pIsAvailable : PVARIANT_BOOL) : HRESULT; stdcall;
		function get_domain(out strDomain : WideString) : HRESULT; stdcall;
		function topMenu : HRESULT; stdcall;
		function titleMenu : HRESULT; stdcall;
		function back : HRESULT; stdcall;
		function resume : HRESULT; stdcall;
	end;
	
	IWMPCore2 = interface(IWMPCore)
		['{BC17E5B7-7561-4c18-BB90-17D485775659}']
		function get_dvd(out ppDVD : IWMPDVD) : HRESULT; stdcall;
	end;
	
	// same as IWMPPlayer2, but inherits from IWMPCore2
	IWMPPlayer3 = interface(IWMPCore2)
		['{54062B68-052A-4c25-A39F-8B63346511D4}']
		function get_enabled(pbEnabled : PVARIANT_BOOL) : HRESULT; stdcall;
		function put_enabled(bEnabled : VARIANT_BOOL) : HRESULT; stdcall;
		function get_fullScreen(pbFullScreen : PVARIANT_BOOL) : HRESULT; stdcall;
		function put_fullScreen(bFullScreen : VARIANT_BOOL) : HRESULT; stdcall;
		function get_enableContextMenu(pbEnableContextMenu : PVARIANT_BOOL) : HRESULT; stdcall;
		function put_enableContextMenu(bEnableContextMenu : VARIANT_BOOL) : HRESULT; stdcall;
		function put_uiMode(bstrMode : WideString) : HRESULT; stdcall;
		function get_uiMode(out pbstrMode : WideString) : HRESULT; stdcall;
		function get_stretchToFit(pbEnabled : PVARIANT_BOOL) : HRESULT; stdcall;
		function put_stretchToFit(bEnabled : VARIANT_BOOL) : HRESULT; stdcall;
		function get_windowlessVideo(pbEnabled : PVARIANT_BOOL) : HRESULT; stdcall;
		function put_windowlessVideo(bEnabled : VARIANT_BOOL) : HRESULT; stdcall;
	end;	

	IWMPErrorItem2 = interface(IWMPErrorItem)
		['{F75CCEC0-C67C-475c-931E-8719870BEE7D}']
		function get_condition(plCondition : PLongint) : HRESULT; stdcall;
	end;
	
	IWMPRemoteMediaServices = interface(IUnknown)
		['{CBB92747-741F-44fe-AB5B-F1A48F3B2A59}']
		function GetServiceType(out pbstrType : WideString) : HRESULT; stdcall;
		function GetApplicationType(out pbstrName : WideString) : HRESULT; stdcall;
		function GetScriptableObject(out pbstrName : WideString; out ppDispatch : IDispatch) : HRESULT; stdcall;
		function GetCustomUIMode(out bstrFile : WideString) : HRESULT; stdcall;
	end;
	
	IWMPSkinManager = interface(IUnknown)
		['{076F2FA6-ED30-448B-8CC5-3F3EF3529C7A}']
		function SetVisualStyle(bstrPath : WideString) : HRESULT; stdcall;
	end;
	
	IWMPMetadataPicture = interface(IDispatch)
		['{5C29BBE0-F87D-4c45-AA28-A70F0230FFA9}']
		function get_mimeType(out pbstrMimeType : WideString) : HRESULT; stdcall;
		function get_pictureType(out pbstrPictureType : WideString) : HRESULT; stdcall;
		function get_description(out pbstrDescription : WideString) : HRESULT; stdcall;
		function get_URL(out pbstrURL : WideString) : HRESULT; stdcall;
	end;
	
	IWMPMetadataText = interface(IDispatch)
		['{769A72DB-13D2-45e2-9C48-53CA9D5B7450}']
		function get_description(out pbstrDescription : WideString) : HRESULT; stdcall;
		function get_text(out pbstrText : WideString) : HRESULT; stdcall;
	end;
	
	IWMPMedia3 = interface(IWMPMedia2)
		['{F118EFC7-F03A-4fb4-99C9-1C02A5C1065B}']
		function getAttributeCountByType(bstrType, bstrLanguage : WideString; plCount : PLongint) : HRESULT; stdcall;
		function getItemInfoByType(bstrType, bstrLanguage : WideString; lIndex : Longint; pvalValue : POleVariant) : HRESULT; stdcall;
	end;
	
	IWMPSettings2 = interface(IWMPSettings)
		['{FDA937A4-EECE-4da5-A0B6-39BF89ADE2C2}']
		function get_defaultAudioLanguage(plLangID : PLongint) : HRESULT; stdcall;
		function get_mediaAccessRights(out pbstrRights : WideString) : HRESULT; stdcall;
		function requestMediaAccessRights(bstrDesiredAccess : WideString; pvbAccepted : PVARIANT_BOOL) : HRESULT; stdcall;
	end;

	IWMPControls3 = interface(IWMPControls2)
		['{A1D1110E-D545-476a-9A78-AC3E4CB1E6BD}']
		function get_audioLanguageCount(plCount : PLongint) : HRESULT; stdcall;
		function getAudioLanguageID(plLangID : PLongint) : HRESULT; stdcall;
		function getAudioLanguageDescription(lIndex : Longint; out pbstrLandDesc : WideString) : HRESULT; stdcall;
		function get_currentAudioLanguage(plLangID : PLongint) : HRESULT; stdcall;
		function put_currentAudioLanguage(lLangID : Longint) : HRESULT; stdcall;
		function get_currentAudioLanguageIndex(plIndex : PLongint) : HRESULT; stdcall;
		function put_currentAudioLanguageIndex(lIndex : Longint) : HRESULT; stdcall;
		function getLanguageName(lLandID : Longint; pbstrLangName : WideString) : HRESULT; stdcall;
		function get_currentPositionTimecode(out bstrTimecode : WideString) : HRESULT; stdcall;
		function put_currentPositionTimecode(bstrTimecode : WideString) : HRESULT; stdcall;
	end;
	
	IWMPClosedCaption2 = interface(IWMPClosedCaption)
		['{350BA78B-6BC8-4113-A5F5-312056934EB6}']
		function get_SAMILangCount(plCount : PLongint) : HRESULT; stdcall;
		function getSAMILangName(nIndex : Longint; out pbstrName : WideString) : HRESULT; stdcall;
		function getSAMILangID(nIndex : Longint; plLangID : PLongint) : HRESULT; stdcall;
		function get_SAMIStyleCount(plCount : PLongint) : HRESULT; stdcall;
		function getSAMIStyleName(nIndex : Longint; out pbstrName : WideString) : HRESULT; stdcall;
	end;

	IWMPPlayerApplication = interface(IDispatch)
		['{40897764-CEAB-47be-AD4A-8E28537F9BBF}']
		function switchToPlayerApplication : HRESULT; stdcall;
		function switchToControl : HRESULT; stdcall;
		function get_playerDocked(pbPlayerDocked : PVARIANT_BOOL) : HRESULT; stdcall;
		function get_hasDisplay(pbHasDisplay : PVARIANT_BOOL) : HRESULT; stdcall;
	end;
	
	IWMPCore3 = interface(IWMPCore2)
		['{7587C667-628F-499f-88E7-6A6F4E888464}']
		function newPlaylist(bstrName, bstrURL : WideString; out ppPlaylist : IWMPPlaylist) : HRESULT; stdcall;
		function newMedia(bstrURL : WideString; out ppMedia : IWMPMedia) : HRESULT; stdcall;
	end;
	
	IWMPPlayer4 = interface(IWMPCore3)
		['{6C497D62-8919-413c-82DB-E935FB3EC584}']
		function get_enabled(pbEnabled : PVARIANT_BOOL) : HRESULT; stdcall;
		function put_enabled(bEnabled : VARIANT_BOOL) : HRESULT; stdcall;
		function get_fullScreen(pbFullScreen : PVARIANT_BOOL) : HRESULT; stdcall;
		function put_fullScreen(bFullScreen : VARIANT_BOOL) : HRESULT; stdcall;
		function get_enableContextMenu(pbEnableContextMenu : PVARIANT_BOOL) : HRESULT; stdcall;
		function put_enableContextMenu(bEnableContextMenu : VARIANT_BOOL) : HRESULT; stdcall;
		function put_uiMode(bstrMode : WideString) : HRESULT; stdcall;
		function get_uiMode(out pbstrMode : WideString) : HRESULT; stdcall;
		function get_stretchToFit(pbEnabled : PVARIANT_BOOL) : HRESULT; stdcall;
		function put_stretchToFit(bEnabled : VARIANT_BOOL) : HRESULT; stdcall;
		function get_windowlessVideo(pbEnabled : PVARIANT_BOOL) : HRESULT; stdcall;
		function put_windowlessVideo(bEnabled : VARIANT_BOOL) : HRESULT; stdcall;
		function get_isRemote(pvarfIsRemote : PVARIANT_BOOL) : HRESULT; stdcall;
		function get_playerApplication(out ppIWMPPlayerApplication : IWMPPlayerApplication) : HRESULT; stdcall;
		function openPlayer(bstrURL : WideString) : HRESULT; stdcall;
	end;
	
	IWMPPlayerServices = interface(IUnknown)
		['{1D01FBDB-ADE2-4c8d-9842-C190B95C3306}']
		function activateUIPlugin(bstrPlugin : WideString) : HRESULT; stdcall;
		function setTaskPane(bstrTaskPane : WideString) : HRESULT; stdcall;
		function setTaskPaneURL(bstrTaskPane, bstrURL, bstrFriendlyName : WideString) : HRESULT; stdcall;
	end;
	
	IWMPSyncDevice = interface(IUnknown)
		['{82A2986C-0293-4fd0-B279-B21B86C058BE}']
		function get_friendlyName(out pbstrName : WideString) : HRESULT; stdcall;
		function put_friendlyName(bstrName : WideString) : HRESULT; stdcall;
		function get_deviceName(out pbstrName : WideString) : HRESULT; stdcall;
		function get_deviceId(out pbstrDeviceId : WideString) : HRESULT; stdcall;
		function get_partnershipIndex(plIndex : PLongint) : HRESULT; stdcall;
		function get_connected(pvbConnected : PVARIANT_BOOL) : HRESULT; stdcall;
		function get_status(pwmpds : PWMPDeviceStatus) : HRESULT; stdcall;
		function get_syncState(pwmpss : PWMPSyncState) : HRESULT; stdcall;
		function get_progress(plProgress : PLongint) : HRESULT; stdcall;
		function getItemInfo(bstrItemName : WideString; out pbstrVal : WideString) : HRESULT; stdcall;
		function createPartnership(vbShowUI : VARIANT_BOOL) : HRESULT; stdcall;
		function deletePartnership : HRESULT; stdcall;
		function start : HRESULT; stdcall;
		function stop : HRESULT; stdcall;
		function showSettings : HRESULT; stdcall;
		function isIdentical(pDevice : IWMPSyncDevice; pvbool : PVARIANT_BOOL) : HRESULT; stdcall;
	end;
	
	IWMPSyncServices = interface(IUnknown)
		['{8B5050FF-E0A4-4808-B3A8-893A9E1ED894}']
		function get_deviceCount(plCount : PLongint) : HRESULT; stdcall;
		function getDevice(lIndex : Longint; out ppDevice : IWMPSyncDevice) : HRESULT; stdcall;
	end;
	
	IWMPPlayerServices2 = interface(IWMPPlayerServices)
		['{1BB1592F-F040-418a-9F71-17C7512B4D70}']
		function setBackgroundProcessingPriority(bstrPriority : WideString) : HRESULT; stdcall;
	end;
	
	IWMPCdromRip = interface(IUnknown)
		['{56E2294F-69ED-4629-A869-AEA72C0DCC2C}']
		function get_ripState(pwmprs : PWMPRipState) : HRESULT; stdcall;
		function get_ripProgress(plProgress : PLongint) : HRESULT; stdcall;
		function startRip : HRESULT; stdcall;
		function stopRip : HRESULT; stdcall;
	end;
	
	IWMPCdromBurn = interface(IUnknown)
		['{BD94DBEB-417F-4928-AA06-087D56ED9B59}']
		function isAvailable(bstrItem : WideString; pIsAvailable : PVARIANT_BOOL) : HRESULT; stdcall;
		function getItemInfo(bstrItem : WideString; out pbstrVal : WideString) : HRESULT; stdcall;
		function get_label(out pbstrLabel : WideString) : HRESULT; stdcall;
		function put_label(bstrLabel : WideString) : HRESULT; stdcall;
		function get_burnFormat(pwmpbf : PWMPBurnFormat) : HRESULT; stdcall;
		function put_burnFormat(wmpbf : WMPBurnFormat) : HRESULT; stdcall;
		function get_burnPlaylist(out ppPlaylist : IWMPPlaylist) : HRESULT; stdcall;
		function put_burnPlaylist(pPlaylist : IWMPPlaylist) : HRESULT; stdcall;
		function refreshStatus : HRESULT; stdcall;
		function get_burnState(pwmpbs : PWMPBurnState) : HRESULT; stdcall;
		function get_burnProgress(plProgress : PLongint) : HRESULT; stdcall;
		function startBurn : HRESULT; stdcall;
		function stopBurn : HRESULT; stdcall;
		function erase : HRESULT; stdcall;
	end;
	
	IWMPQuery = interface(IDispatch)
		['{a00918f3-a6b0-4bfb-9189-fd834c7bc5a5}']
		function addCondition(bstrAttribute, bstrOperator, bstrValue : WideString) : HRESULT; stdcall;
		function beginNextGroup : HRESULT; stdcall;
	end;
	
	IWMPMediaCollection2 = interface(IWMPMediaCollection)
		['{8BA957F5-FD8C-4791-B82D-F840401EE474}']
		function createQuery(out ppQuery : IWMPQuery) : HRESULT; stdcall;
		function getPlaylistByQuery(pQuery : IWMPQuery; bstrMediaType, bstrSortAttribute : WideString; fSortAscending : VARIANT_BOOL; out ppPlaylist : IWMPPlaylist) : HRESULT; stdcall;
		function getStringCollectionByQuery(pQuery : IWMPQuery; bstrMediaType, bstrSortAttribute : WideString; fSortAscending : VARIANT_BOOL; out ppStringCollection : IWMPStringCollection) : HRESULT; stdcall;
		function getByAttributeAndMediaType(bstrAttribute, bstrValue, bStrMediaType : WideString; out ppMediaItems : IWMPPlaylist) : HRESULT; stdcall;
	end;
	
	IWMPStringCollection2 = interface(IWMPStringCollection)
		['{46ad648d-53f1-4a74-92e2-2a1b68d63fd4}']
		function isIdentical(pIWMPStringCollection2 : IWMPStringCollection2; pvbool : PVARIANT_BOOL) : HRESULT; stdcall;
		function getItemInfo(lCollectionIndex : Longint; bstrItemName : WideString; out pbstrValue : WideString) : HRESULT; stdcall;
		function getAttributeCountByType(lCollectionIndex : Longint; bstrType, bstrLanguage : WideString; plCount : PLongint) : HRESULT; stdcall;
		function getItemInfoByType(lCollectionIndex : Longint; bstrType, bstrLanguage : WideString; lAttributeIndex : Longint; pvarValue : POleVariant) : HRESULT; stdcall;
	end;
	
	IWMPLibrary = interface(IUnknown)
		['{3DF47861-7DF1-4c1f-A81B-4C26F0F7A7C6}']
		function get_name(out pbstrName : WideString) : HRESULT; stdcall;
		function get_type(pwmplt : PWMPLibraryType) : HRESULT; stdcall;
		function get_mediaCollection(out ppIWMPMediaCollection : IWMPMediaCollection) : HRESULT; stdcall;
		function isIdentical(pIWMPLibrary : IWMPLibrary; pvbool : PVARIANT_BOOL) : HRESULT; stdcall;
	end;
	
	IWMPLibraryServices = interface(IUnknown)
		['{39C2F8D5-1CF2-4d5e-AE09-D73492CF9EAA}']
		function getCountByType(wmplt : WMPLibraryType; plCount : PLongint) : HRESULT; stdcall;
		function getLibraryByType(wmplt : WMPLibraryType; lIndex : Longint; out ppIWMPLibrary : IWMPLibrary) : HRESULT; stdcall;
	end;
	
	IWMPLibrarySharingServices = interface(IUnknown)
		['{82CBA86B-9F04-474b-A365-D6DD1466E541}']
		function isLibraryShared(pvbShared : PVARIANT_BOOL) : HRESULT; stdcall;
		function isLibrarySharingEnabled(pvbEnabled : PVARIANT_BOOL) : HRESULT; stdcall;
		function showLibrarySharing : HRESULT; stdcall;
	end;
	
	IWMPFolderMonitorServices = interface(IUnknown)
		['{788C8743-E57F-439d-A468-5BC77F2E59C6}']
		function get_count(plCount : PLongint) : HRESULT; stdcall;
		function item(lIndex : Longint; out pbstrFolder : WideString) : HRESULT; stdcall;
		function add(bstrFolder : WideString) : HRESULT; stdcall;
		function remove(lIndex : Longint) : HRESULT; stdcall;
		function get_scanState(pwmpfss : PWMPFolderScanState) : HRESULT; stdcall;
		function get_currentFolder(out pbstrFolder : WideString) : HRESULT; stdcall;
		function get_scannedFilesCount(plCount : PLongint) : HRESULT; stdcall;
		function get_addedFilesCount(plCount : PLongint) : HRESULT; stdcall;
		function get_updateProgress(plProgress : PLongint) : HRESULT; stdcall;
		function startScan : HRESULT; stdcall;
		function stopScan : HRESULT; stdcall;
	end;
	
	IWMPSyncDevice2 = interface(IWMPSyncDevice)
		['{88AFB4B2-140A-44d2-91E6-4543DA467CD1}']
		function setItemInfo(bstrItemName, bstrVal : WideString) : HRESULT; stdcall;
	end;
	
	IWMPEvents = interface(IUnknown)
		['{19A6627B-DA9E-47c1-BB23-00B5E668236A}']
		procedure OpenStateChange(NewState : Longint); stdcall;
		procedure PlayStateChange(NewState : Longint); stdcall;
		procedure AudioLanguageChange(LangID : Longint); stdcall;
		procedure StatusChange; stdcall;
		procedure ScriptCommand(scType, Param : WideString); stdcall;
		procedure NewStream; stdcall;
		procedure Disconnect(_Result : Longint); stdcall;
		procedure Buffering(Start : VARIANT_BOOL); stdcall;
		procedure Error; stdcall;
		procedure Warning(WarningType, Param : Longint; Description : WideString); stdcall;
		procedure EndOfStream(_Result : Longint); stdcall;
		procedure PositionChange(oldPosition, newPosition : Double); stdcall;
		procedure MarkerHit(MarkerNum : Longint); stdcall;
		procedure DurationUnitChange(NewDurationUnit : Longint); stdcall;
		procedure CdromMediaChange(CdromNum : Longint); stdcall;
		procedure PlaylistChange(Playlist : IDispatch; change : WMPPlaylistChangeEventType); stdcall;
		procedure CurrentPlaylistChange(change : WMPPlaylistChangeEventType); stdcall;
		procedure CurrentPlaylistItemAvailable(bstrItemName : WideString); stdcall;
		procedure MediaChange(Item : IDispatch); stdcall;
		procedure CurrentMediaItemAvailable(bstrItemName : WideString); stdcall;
		procedure CurrentItemChange(pdispMedia : IDispatch); stdcall;
		procedure MediaCollectionChange; stdcall;
		procedure MediaCollectionAttributeStringAdded(bstrAttribName, bstrOldAttribVal : WideString); stdcall;
		procedure MediaCollectionAttributeStringRemoved(bstrAttribName, bstrOldAttribVal : WideString); stdcall;
		procedure MediaCollectionAttributeStringChanged(bstrAttribName, bstrOldAttribVal, bstrNewAttribVal : WideString); stdcall;
		procedure PlaylistCollectionChange; stdcall;
		procedure PlaylistCollectionPlaylistAdded(bstrPlaylistName : WideString); stdcall;
		procedure PlaylistCollectionPlaylistRemoved(bstrPlaylistName : WideString); stdcall;
		procedure PlaylistCollectionPlaylistSetAsDeleted(bstrPlaylistName : WideString; varfIsDeleted : VARIANT_BOOL); stdcall;
		procedure ModeChange(ModeName : WideString; NewValue : VARIANT_BOOL); stdcall;
		procedure MediaError(pMediaObject : IDispatch); stdcall;
		procedure OpenPlaylistSwitch(pItem : IDispatch); stdcall;
		procedure DomainChange(strDomain : WideString); stdcall;
		procedure SwitchedToPlayerApplication; stdcall;
		procedure SwitchedToControl; stdcall;
		procedure PlayerDockedStateChange; stdcall;
		procedure PlayerReconnect; stdcall;
		procedure Click(nButton, nShiftState : Smallint; fX, fY : Longint); stdcall;
		procedure DoubleClick(nButton, nShiftState : Smallint; fX, fY : Longint); stdcall;
		procedure KeyDown(nKeyCode, nShiftState : Smallint); stdcall;
		procedure KeyPress(nKeyAscii : Smallint); stdcall;
		procedure KeyUp(nKeyCode, nShiftState : Smallint); stdcall;
		procedure MouseDown(nButton, nShiftState : Smallint; fX, fY : Longint); stdcall;
		procedure MouseMove(nButton, nShiftState : Smallint; fX, fY : Longint); stdcall;
		procedure MouseUp(nButton, nShiftState : Smallint; fX, fY : Longint); stdcall;
	end;
	
	IWMPEvents2 = interface(IWMPEvents)
		['{1E7601FA-47EA-4107-9EA9-9004ED9684FF}']
		procedure DeviceConnect(pDevice : IWMPSyncDevice); stdcall;
		procedure DeviceDisconnect(pDevice : IWMPSyncDevice); stdcall;
		procedure DeviceStatusChange(pDevice : IWMPSyncDevice; NewStatus : WMPDeviceStatus); stdcall;
		procedure DeviceSyncStateChange(pDevice : IWMPSyncDevice; NewState : WMPSyncState); stdcall;
		procedure DeviceSyncError(pDevice : IWMPSyncDevice; pMedia : IDispatch); stdcall;
		procedure CreatePartnershipComplete(pDevice : IWMPSyncDevice; hrResult : HRESULT); stdcall;
	end;
	
	IWMPEvents3 = interface(IWMPEvents2)
		['{1F504270-A66B-4223-8E96-26A06C63D69F}']
		procedure CdromRipStateChange(pCdromRip : IWMPCdromRip; wmprs : WMPRipState); stdcall;
		procedure CdromRipMediaError(pCdromRip : IWMPCdromRip; pMedia : IDispatch); stdcall;
		procedure CdromBurnStateChange(pCdromBurn : IWMPCdromBurn; wmpbs : WMPBurnState); stdcall;
		procedure CdromBurnMediaError(pCdromBurn : IWMPCdromBurn; pMedia : IDispatch); stdcall;
		procedure CdromBurnError(pCdromBurn : IWMPCdromBurn; hrError : HRESULT); stdcall;
		procedure LibraryConnect(pLibrary : IWMPLibrary); stdcall;
		procedure LibraryDisconnect(pLibrary : IWMPLibrary); stdcall;
		procedure FolderScanStateChange(wmpfss : WMPFolderScanState); stdcall;
		procedure StringCollectionChange(pdispStringCollection : IDispatch; change : WMPStringCollectionChangeEventType; lCollectionIndex : Longint); stdcall;
		procedure MediaCollectionMediaAdded(pdispMedia : IDispatch); stdcall;
		procedure MediaCollectionMediaRemoved(pdispMedia : IDispatch); stdcall;
	end;
	
	_WMPOCXEvents = interface(IDispatch)
		['{6BF52A51-394A-11d3-B153-00C04F79FAA6}']
	end;
	
const
	CLSID_WindowsMediaPlayer : TGUID = (Data1: $6BF52A52; Data2: $394A; Data3: $11d3; Data4: ($B1, $53, $00, $C0, $4F, $79, $FA, $A6) );
	CLSID_WMPSkinManager : TGUID = (Data1: $B2A7FD52; Data2: $301F; Data3: $4348; Data4: ($B9, $3A, $63, $8C, $6D, $E4, $92, $29) );
		
implementation

end.