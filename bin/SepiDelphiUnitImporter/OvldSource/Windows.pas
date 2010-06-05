Around line 1570 in Delphi 2009
----------
  TTokenInformationClass = (
    TokenUser = 1,
    TokenGroups,
    TokenPrivileges,
    TokenOwner,
    TokenPrimaryGroup,
    TokenDefaultDacl,
    TokenSource,
    TokenType,
    TokenImpersonationLevel,
    TokenStatistics,
    TokenRestrictedSids, 
    TokenSessionId,
    TokenGroupsAndPrivileges,
    TokenSessionReference,
    TokenSandBoxInert,
    TokenAuditPolicy,
    TokenOrigin,
    TokenElevationType,
    TokenLinkedToken,
    TokenElevation,
    TokenHasRestrictions,
    TokenAccessInformation,
    TokenVirtualizationAllowed,
    TokenVirtualizationEnabled,
    TokenIntegrityLevel,
    TokenUIAccess,
    TokenMandatoryPolicy,
    TokenLogonSid,
    vMaxTokenInfoClass          );
----------
  TTokenInformationClass = type Byte; const
    TokenUser = TTokenInformationClass(1);
    TokenGroups = TTokenInformationClass(2);
    TokenPrivileges = TTokenInformationClass(3);
    TokenOwner = TTokenInformationClass(4);
    TokenPrimaryGroup = TTokenInformationClass(5);
    TokenDefaultDacl = TTokenInformationClass(6);
    TokenSource = TTokenInformationClass(7);
    TokenType = TTokenInformationClass(8);
    TokenImpersonationLevel = TTokenInformationClass(9);
    TokenStatistics = TTokenInformationClass(10);
    TokenRestrictedSids = TTokenInformationClass(11);
    TokenSessionId = TTokenInformationClass(12);
    TokenGroupsAndPrivileges = TTokenInformationClass(13);
    TokenSessionReference = TTokenInformationClass(14);
    TokenSandBoxInert = TTokenInformationClass(15);
    TokenAuditPolicy = TTokenInformationClass(16);
    TokenOrigin = TTokenInformationClass(17);
    TokenElevationType = TTokenInformationClass(18);
    TokenLinkedToken = TTokenInformationClass(19);
    TokenElevation = TTokenInformationClass(20);
    TokenHasRestrictions = TTokenInformationClass(21);
    TokenAccessInformation = TTokenInformationClass(22);
    TokenVirtualizationAllowed = TTokenInformationClass(23);
    TokenVirtualizationEnabled = TTokenInformationClass(24);
    TokenIntegrityLevel = TTokenInformationClass(25);
    TokenUIAccess = TTokenInformationClass(26);
    TokenMandatoryPolicy = TTokenInformationClass(27);
    TokenLogonSid = TTokenInformationClass(28);
    vMaxTokenInfoClass = TTokenInformationClass(29); type
----------

Around line 1611 in Delphi 2009
----------
  TTokenElevationType = (
    TokenElevationTypeDefault = 1,
    TokenElevationTypeFull,
    TokenElevationTypeLimited
  );
----------
  TTokenElevationType = type Byte; const
    TokenElevationTypeDefault = TTokenElevationType(1);
    TokenElevationTypeFull = TTokenElevationType(2);
    TokenElevationTypeLimited = TTokenElevationType(3); type
----------

