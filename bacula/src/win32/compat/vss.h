/*                               -*- Mode: C -*-
 * vss.h --
 */
//
// Copyright transferred from MATRIX-Computer GmbH to
//   Kern Sibbald by express permission.
//
//  Copyright (C) 2005-2006 Kern Sibbald
//
//  This program is free software; you can redistribute it and/or
//  modify it under the terms of the GNU General Public License
//  version 2 as amended with additional clauses defined in the
//  file LICENSE in the main source directory.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the 
//  the file LICENSE for additional details.
//

/*
 *
 * Author          : Thorsten Engel
 * Created On      : Fri May 06 21:44:00 2006 
 */

#ifndef __VSS_H_
#define __VSS_H_

#ifndef b_errno_win32
#define b_errno_win32 (1<<29)
#endif
 
#ifdef WIN32_VSS

// some forward declarations
struct IVssAsync;

class VSSClient
{
public:
    VSSClient();
    ~VSSClient();

    // Backup Process
    BOOL InitializeForBackup();
    virtual BOOL CreateSnapshots(char* szDriveLetters) = 0;
    virtual BOOL CloseBackup() = 0;
    virtual const char* GetDriverName() = 0;
    BOOL GetShadowPath  (const char* szFilePath, char* szShadowPath, int nBuflen);
    BOOL GetShadowPathW (const wchar_t* szFilePath, wchar_t* szShadowPath, int nBuflen); /* nBuflen in characters */

    const size_t GetWriterCount();
    const char* GetWriterInfo(int nIndex);
    const int   GetWriterState(int nIndex);
    void DestroyWriterInfo();
    void AppendWriterInfo(int nState, const char* pszInfo);
    const BOOL  IsInitialized() { return m_bBackupIsInitialized; };
         
private:
    virtual BOOL Initialize(DWORD dwContext, BOOL bDuringRestore = FALSE) = 0;
    virtual BOOL WaitAndCheckForAsyncOperation(IVssAsync*  pAsync) = 0;
    virtual void QuerySnapshotSet(GUID snapshotSetID) = 0;

protected:
    HMODULE                         m_hLib;

    BOOL                            m_bCoInitializeCalled;
    BOOL                            m_bCoInitializeSecurityCalled;
    DWORD                           m_dwContext;

    IUnknown*                       m_pVssObject;
    GUID                            m_uidCurrentSnapshotSet;
    // TRUE if we are during restore
    BOOL                            m_bDuringRestore;
    BOOL                            m_bBackupIsInitialized;

    // drive A will be stored on position 0,Z on pos. 25
    wchar_t                           m_wszUniqueVolumeName[26][MAX_PATH]; // approx. 7 KB
    wchar_t                           m_szShadowCopyName[26][MAX_PATH]; // approx. 7 KB
    
    void*                           m_pAlistWriterState;
    void*                           m_pAlistWriterInfoText;
};

class VSSClientXP:public VSSClient
{
public:
   VSSClientXP();
   virtual ~VSSClientXP();
   virtual BOOL CreateSnapshots(char* szDriveLetters);
   virtual BOOL CloseBackup();
   virtual const char* GetDriverName() { return "VSS WinXP"; };
private:
   virtual BOOL Initialize(DWORD dwContext, BOOL bDuringRestore);
   virtual BOOL WaitAndCheckForAsyncOperation(IVssAsync* pAsync);
   virtual void QuerySnapshotSet(GUID snapshotSetID);
   BOOL CheckWriterStatus();   
};

class VSSClient2003:public VSSClient
{
public:
   VSSClient2003();
   virtual ~VSSClient2003();
   virtual BOOL CreateSnapshots(char* szDriveLetters);
   virtual BOOL CloseBackup();   
   virtual const char* GetDriverName() { return "VSS Win 2003"; };
private:
   virtual BOOL Initialize(DWORD dwContext, BOOL bDuringRestore);
   virtual BOOL WaitAndCheckForAsyncOperation(IVssAsync*  pAsync);
   virtual void QuerySnapshotSet(GUID snapshotSetID);
   BOOL CheckWriterStatus();
};

#endif /* WIN32_VSS */

#endif /* __VSS_H_ */
