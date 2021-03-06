unit smmGlobal;

interface

{$Include smmOptions.inc}

uses
  ScaleMM2, smmTypes,
  smmSmallMemory, smmMediumMemory;

type
  /// Global memory manager
  // - a single instance is created for the whole process
  // - caches some memory (blocks + threadmem) for fast reuse
  // - also keeps allocated memory in case an old thread allocated some memory
  // for another thread
  PGlobalMemManager = ^TGlobalMemManager;
  TGlobalMemManager = object
  private {locks}
    //FBlockLock_old: NativeUInt;
    FFreeBlockCount: NativeUInt;
    FThreadLock: NativeUInt;
    FThreadLockRecursion: NativeUInt;
  private {threads}
    /// all thread memory managers
    //FFirstThreadMemory: PThreadMemManager;
    /// freed/used thread memory managers
    // - used to cache the per-thread managers in case of multiple threads creation
    FFirstFreedThreadMemory: PThreadMemManager;
    FFirstThreadMemory: PThreadMemManager;
  private
    FSmallInterThreadMemCount: NativeUInt;
  private {small}
    /// global thread manager (owner of all global mem)
    FGlobalThreadMemory: PThreadMemManager;
  private {medium}
    FFirstBlock: PMediumBlockMemory;
  protected
    procedure FreeSmallBlocksFromThreadMemory(aThreadMem: PSmallMemThreadManager);
    procedure FreeMediumBlocksFromThreadMemory(aThreadMem: PMediumThreadManager);
  public
    procedure Init;

    function  TryThreadLock: boolean;
    procedure ThreadLock;
    procedure ThreadUnLock;

    function  GetNewThreadManager: PThreadMemManager;
    //procedure AddNewThreadManagerToList(aThreadMem: PThreadMemManager);
    function  GetFirstThreadMemory: PThreadMemManager;
    procedure FreeThreadManager(aThreadMem: PThreadMemManager);
    procedure FreeAllMemory;

    procedure StopBackGroundThread;  // 高凉新农+
    procedure FreeBackGroundThread;  // 高凉新农+
    
    procedure FreeMediumBlockMemory(aBlockMem: PMediumBlockMemory);
    function  GetMediumBlockMemory(aNewOwner: PMediumThreadManager; aMinResultSize: NativeUInt): PMediumBlockMemory;

    //procedure FreeSmallBlockMemory(aBlockMem: PSmallMemBlock);
    function  GetSmallBlockMemory(aItemSize: NativeUInt): PSmallMemBlock;
    procedure IncSmallInterthreadMem;

    procedure CheckSmallMem;
    procedure ProcessFreedMemoryFromOtherThreads;
  end;
//{$A+}?

  TScaleMMBackGroundThread = class
  protected
    FHandle: THandle;
    FTerminated : Boolean;
    FFinished : Boolean;
    procedure Execute;
  public
    constructor Create;
    destructor Destroy; override;
    procedure StopThread;
    class function GarbageCollectionActive: boolean;
  end;

var
  GlobalManager: PGlobalMemManager;         
  OwnedGlobalManager: TGlobalMemManager;    //needed for ShareMM

implementation

uses
  smmFunctions;

var
  BGThread: TScaleMMBackGroundThread;

{ TGlobalMemManager }

(*
procedure TGlobalMemManager.AddNewThreadManagerToList(aThreadMem: PThreadMemManager);
var
  pprevthreadmem: PThreadMemManager;
begin
  repeat
    pprevthreadmem := FFirstThreadMemory;
    // try to set "result" in global var
    if CAS32(pprevthreadmem, aThreadMem, FFirstThreadMemory) then
      Break;
    if not SwitchToThread then
      sleep(0);
    pprevthreadmem := FFirstThreadMemory;
    if CAS32(pprevthreadmem, aThreadMem, FFirstThreadMemory) then
      Break;
    Sleep(1);
  until false;
  // make linked list: new one is first item (global var), next item is previous item
  aThreadMem.FNextThreadManager := pprevthreadmem;
end;
*)

procedure TGlobalMemManager.CheckSmallMem;
begin
  //FGlobalThreadMemory.FSmallMemManager.
end;

procedure TGlobalMemManager.FreeAllMemory;

(*
  procedure __ProcessBlockMem(aOldBlock: PSmallMemBlockList);
  var
    allmem, oldmem: PSmallMemBlock;
  begin
    if aOldBlock = nil then
      Exit;
    allmem := aOldBlock.FFirstFreedMemBlock;
    while allmem <> nil do
    begin
      // not in use
      if allmem.FUsageCount = allmem.FFreedIndex then
      begin
        oldmem := allmem;
        allmem := allmem.FNextFreedMemBlock;
        Scale_FreeMem(oldmem);
        {FMainThreadMemory.}Scale_FreeMem(oldmem);
      end
      else
        allmem := allmem.FNextFreedMemBlock;
    end;
  end;
  *)

var
  oldthreadmem, tempthreadmem: PThreadMemManager;
  medblock, mednextblock: PMediumBlockMemory;
  medfirstmem : PMediumHeader;
//  i: NativeUInt;
begin
  oldthreadmem := Self.FFirstFreedThreadMemory;
  while oldthreadmem <> nil do
  begin
    tempthreadmem := oldthreadmem;
    oldthreadmem  := oldthreadmem.FNextFreeThreadManager;

    tempthreadmem.FSmallMemManager.FreeThreadFreedMem;
    //get all pending memory and add it to our global manager
    FreeSmallBlocksFromThreadMemory(@tempthreadmem.FSmallMemManager);
    FreeMediumBlocksFromThreadMemory(@tempthreadmem.FMediumMemManager);
    //process all interthread memory (because our global manager is the owner now, it is just forwarded to this global manager)
    tempthreadmem.ProcessFreedMemFromOtherThreads(false);
    //clear
    tempthreadmem.Reset;
  end;

  //process pending + forwarded interthread memory
  ProcessFreedMemoryFromOtherThreads;

  //clear cached/internal memory of the (sub)managers
  FGlobalThreadMemory.ReleaseAllFreeMem;

  //free cached medium blocks
  medblock := Self.FFirstBlock;
  while medblock <> nil do
  begin
    mednextblock := medblock.NextBlock;

    medfirstmem := PMediumHeader( NativeUInt(medblock) + SizeOf(TMediumBlockMemory));
    //is free mem?
    if medfirstmem.Size and 1 <> 0 then
      //fully free mem? we can only release fully free mem (duh...)
      if PMediumHeaderExt(medfirstmem).ArrayPosition = 16 then
        //RELEASE TO WINDOWS
        VirtualFree(medblock, 0 {all}, MEM_RELEASE);

    medblock := mednextblock;
  end;


  // free internal blocks
//  for i := Low(Self.FFreedMiniMemoryBlocks) to High(Self.FFreedMiniMemoryBlocks) do
//    __ProcessBlockMem(@Self.FFreedMiniMemoryBlocks[i]);
//  for i := Low(Self.FFreedSmallMemoryBlocks) to High(Self.FFreedSmallMemoryBlocks) do
//    __ProcessBlockMem(@Self.FFreedSmallMemoryBlocks[i]);

  { TODO -oAM : release small mem }
  (*
  // free current thread
  tempthreadmem := ScaleMM.GetThreadMemManager;
  with tempthreadmem do
  begin
    for i := Low(tempthreadmem. FMiniMemoryBlocks) to High(tempthreadmem.FMiniMemoryBlocks) do
      __ProcessBlockMem(@tempthreadmem.FMiniMemoryBlocks[i]);
    for i := Low(tempthreadmem.FSmallMemoryBlocks) to High(tempthreadmem.FSmallMemoryBlocks) do
      __ProcessBlockMem(@tempthreadmem.FSmallMemoryBlocks[i]);
  end;
  *)

  { TODO -oAM : release medium mem }

  // free cached threads
  oldthreadmem := Self.FFirstFreedThreadMemory;
  while oldthreadmem <> nil do
  begin
    tempthreadmem := oldthreadmem;
    oldthreadmem  := oldthreadmem.FNextFreeThreadManager;
    VirtualFree(tempthreadmem, 0, MEM_RELEASE);
  end;
end;

procedure TGlobalMemManager.FreeMediumBlockMemory(
  aBlockMem: PMediumBlockMemory);
var
  firstmem: PMediumHeader;
  blocked: boolean;
//  freeheader: PMediumHeaderExt;
begin
  blocked := TryThreadlock;

  //keep max 10 blocks in buffer
  if (FFreeBlockCount >= 10) or not blocked then
  begin
    firstmem := PMediumHeader( NativeUInt(aBlockMem) + SizeOf(TMediumBlockMemory));
    //is free mem?
    //if NativeUInt(firstmem.NextMem) > NativeUInt(1 shl 31) then
    if (firstmem.Size and 1 <> 0) or
       //check if fully free, if so, it will be released
       (aBlockMem.GetMaxFreeSizeInBlock = C_MAX_MEDIUMMEM_SIZE) then
    begin
      //fully free mem? we can only release fully free mem (duh...)
      if PMediumHeaderExt(firstmem).ArrayPosition = 16 then
      begin
        if blocked then
          ThreadUnLock;

        //RELEASE TO WINDOWS
        VirtualFree(aBlockMem, 0 {all}, MEM_RELEASE);
        //exit!
        Exit;
      end;
    end;
    //(False);
  end;

  if not blocked then ThreadLock;
  try
    ProcessFreedMemoryFromOtherThreads;

    //LOCK
    {
    while not CAS32(0, 1, @FBlockLock) do
    begin
      //small wait: try to swith to other pending thread (if any) else direct continue
      if not SwitchToThread then
        sleep(0);
      //try again
      if CAS32(0, 1, @FBlockLock) then
        Break;
      //wait some longer: force swith to any other thread
      sleep(1);
    end;
    }

    aBlockMem.ChangeOwnerThread(@Self.FGlobalThreadMemory.FMediumMemManager);

    //linked list of thread blocks: replace first item
    if FFirstBlock <> nil then
      FFirstBlock.PreviousBlock := aBlockMem;
    aBlockMem.NextBlock         := FFirstBlock;
    aBlockMem.PreviousBlock     := nil;
    FFirstBlock                 := aBlockMem;
    inc(FFreeBlockCount);
  finally
    //UNLOCK
    //if not CAS32(1, 0, @FBlockLock) then
    //  Assert(False);
    //FBlockLock := 0;
    ThreadUnlock;
  end;
end;

procedure TGlobalMemManager.FreeMediumBlocksFromThreadMemory(
  aThreadMem: PMediumThreadManager);
var
  threadblock, nextblock: PMediumBlockMemory;
begin
  threadblock := aThreadMem.FFirstBlock;
  aThreadMem.FFirstBlock := nil;

  while threadblock <> nil do
  begin
    nextblock   := threadblock.NextBlock;
    FreeMediumBlockMemory(threadblock);
    threadblock := nextblock;
  end;
end;

(*
procedure TGlobalMemManager.FreeSmallBlockMemory(aBlockMem: PSmallMemBlock);
begin
  Assert( aBlockMem.FFreedIndex = aBlockMem.FUsageCount );
  // dispose
  Scale_FreeMem(aBlockMem);

  ProcessFreedMemoryFromOtherThreads;
end;
*)

procedure TGlobalMemManager.FreeSmallBlocksFromThreadMemory(
  aThreadMem: PSmallMemThreadManager);
begin
  aThreadMem.MoveAllMemToOtherManager(@FGlobalThreadMemory.FSmallMemManager);
end;

procedure TGlobalMemManager.FreeThreadManager(aThreadMem: PThreadMemManager);
//var
//  iPrev: Cardinal;
begin
  aThreadMem.FThreadTerminated := True;

  { TODO -oAM : Make GC thread which processes all freed mem in background, now only one FreeThreadManager or FreeInterThreadMemory can be active at a time}

  //LOCK: no threads may be proceseed now (e.g. FreeInterThreadMemory)
  ThreadLock;
    // clear mem (partial: add to reuse list, free = free)
    FreeSmallBlocksFromThreadMemory(@aThreadMem.FSmallMemManager);
    //collect all pending threadfreed memory
    Self.FGlobalThreadMemory.FSmallMemManager.CollectAllThreadFreedMem;
  //UNLOCK
  ThreadUnLock;

  FreeMediumBlocksFromThreadMemory(@aThreadMem.FMediumMemManager);
  aThreadMem.Reset;

  ThreadLock;
  { TODO : keep max nr of threads. Remember to lock "FreeInterThreadMemory" then }
  // add to available list
  aThreadMem.FNextFreeThreadManager := FFirstFreedThreadMemory;
  FFirstFreedThreadMemory := aThreadMem;

  //process mem from other threads
  ProcessFreedMemoryFromOtherThreads;

  { mark as readonly to check writes to manager after it is terminated
    however no linked list like FNextThreadManager can be used then...
  if not VirtualProtect(aThreadMem, SizeOf(TThreadMemManager), PAGE_READONLY, iPrev) then
  begin
    iPrev := GetLastError;
    RaiseLastOSError;
    Assert(False);
  end;
  }

  //UNLOCK
  ThreadUnLock;
end;

function TGlobalMemManager.GetSmallBlockMemory(aItemSize: NativeUInt): PSmallMemBlock;
var bl: PSmallMemBlockList;
begin
  Result := nil;
  bl := Self.FGlobalThreadMemory.FSmallMemManager.GetBlockListOfSize(aItemSize - 1);
  if bl.FFirstFreedMemBlock = nil then Exit;

  ThreadLock;
  try
    //in the mean time some inuse memory can be freed in an other thread
    ProcessFreedMemoryFromOtherThreads;

    (*
    //LOCK
    while not CAS32(0, 1, @FBlockLock) do
    begin
      //small wait: try to swith to other pending thread (if any) else direct continue
      if not SwitchToThread then
        sleep(0);
      //try again
      if CAS32(0, 1, @FBlockLock) then
        Break;
      //wait some longer: force swith to any other thread
      sleep(1);
    end;
    *)

    bl := Self.FGlobalThreadMemory.FSmallMemManager.GetBlockListOfSize(aItemSize - 1);
    // get freed mem from list from front (replace first item)
    if bl.FFirstFreedMemBlock <> nil then
    begin
      Result                 := bl.FFirstFreedMemBlock;
      bl.FFirstFreedMemBlock := Result.FNextFreedMemBlock;
      // remove from linked list
      if Result.FPreviousMemBlock <> nil then
        Result.FPreviousMemBlock.FNextMemBlock := Result.FNextMemBlock;
      if Result.FNextMemBlock <> nil then
        Result.FNextMemBlock.FPreviousMemBlock := Result.FPreviousMemBlock;
      Assert(Result.FPreviousFreedMemBlock = nil);
//      if Result.FPreviousFreedMemBlock <> nil then
//        Result.FPreviousFreedMemBlock.FNextFreedMemBlock := Result.FNextFreedMemBlock;
//      if Result.FNextFreedMemBlock <> nil then
//        Result.FNextFreedMemBlock.FPreviousFreedMemBlock := Result.FPreviousFreedMemBlock;
      if bl.FFirstFreedMemBlock <> nil then
        bl.FFirstFreedMemBlock.FPreviousFreedMemBlock := nil;
      {
        if tempmem.FPreviousFreedMemBlock <> nil then
          tempmem.FPreviousFreedMemBlock.FNextFreedMemBlock := tempmem.FNextFreedMemBlock;
      }
      if Result = bl.FFirstMemBlock then
      begin
        bl.FFirstMemBlock := Result.FNextMemBlock;
        if bl.FFirstMemBlock <> nil then
          bl.FFirstMemBlock.FPreviousMemBlock := nil;
      end;

      {$IFDEF SCALEMM_DEBUG}
      if bl.FFirstMemBlock <> nil then
        bl.FFirstMemBlock.CheckMem(sdBoth);
      if bl.FFirstFreedMemBlock <> nil then
        bl.FFirstFreedMemBlock.CheckMem(sdBoth);
      {$endif}
    end;

    if Result <> nil then
    begin
      {$IFDEF SCALEMM_DEBUG}
      //Result.Lock;
      Result.OwnerThreadId := 2;
      Result.OwnerList     := Pointer(1);
      Result.OwnerManager  := Pointer(2);
      //Result.UnLock;
      {$ENDIF}
      Result.FNextFreedMemBlock      := nil;
      Result.FNextMemBlock           := nil;
      Result.FPreviousMemBlock       := nil;
      Result.FPreviousFreedMemBlock  := nil;
    end;
  finally
    //UNLOCK
    //if not CAS32(1, 0, @FBlockLock) then
    //  Assert(False);
    ThreadUnlock;
  end;
end;

function TGlobalMemManager.GetFirstThreadMemory: PThreadMemManager;
begin
  Result := FFirstThreadMemory;
end;

function TGlobalMemManager.GetMediumBlockMemory(aNewOwner: PMediumThreadManager; aMinResultSize: NativeUInt): PMediumBlockMemory;
begin
  Result := nil;
  if FFirstBlock = nil then Exit;

  //if not TryThreadlock then    this can give OoM in case of high load!
  //  Exit;
  ThreadLock;    //always lock, is a bit slower but otherwise OoM possible
  try
    ProcessFreedMemoryFromOtherThreads;

    //LOCK
    {
    while not CAS32(0, 1, @FBlockLock) do
    begin
      //small wait: try to swith to other pending thread (if any) else direct continue
      if not SwitchToThread then
        sleep(0);
      //try again
      if CAS32(0, 1, @FBlockLock) then
        Break;
      //wait some longer: force swith to any other thread
      sleep(1);
    end;
    }

    //get block
    Result := FFirstBlock;
    //got a block?
    while Result <> nil do
    begin
      //has enough free mem?
      if FGlobalThreadMemory.FMediumMemManager.ScanBlockForFreeItems(Result, aMinResultSize, True {only check size}) = nil then
      begin
        //no, try next block
        Result := Result.NextBlock;
        Continue;
      end;

      //unlink
      if Result.PreviousBlock <> nil then
        Result.PreviousBlock.NextBlock := Result.NextBlock;
      if Result.NextBlock <> nil then
        Result.NextBlock.PreviousBlock := Result.PreviousBlock;
      //rearrange linked list (replace first item)
      if Self.FFirstBlock = Result then
      begin
        Self.FFirstBlock := Result.NextBlock;
        if FFirstBlock <> nil then
          FFirstBlock.PreviousBlock := nil;
      end;

      dec(FFreeBlockCount);
      Break;
    end;

    //got a block?
    if Result <> nil then
    begin
      Result.NextBlock     := nil;
      Result.PreviousBlock := nil;
      Result.ChangeOwnerThread(aNewOwner);
    end;

  finally
    //UNLOCK
    //if not CAS32(1, 0, @FBlockLock) then
    //  Assert(False);
    //FBlockLock := 0;
    ThreadUnlock;
  end;
end;

function TGlobalMemManager.GetNewThreadManager: PThreadMemManager;
begin
  Result := nil;

  //reuse?
  if FFirstFreedThreadMemory <> nil then
  begin
    ThreadLock;

    Result := FFirstFreedThreadMemory;
    if Result <> nil then
    begin
      FFirstFreedThreadMemory       := Result.FNextFreeThreadManager;
      Result.FNextFreeThreadManager := nil;
      Result.Reset;
    end;

    ThreadUnLock;
  end;

  //create new one
  if Result = nil then
  begin
    Result := VirtualAlloc( nil,
                            //64 * 1024,
                            SizeOf(TThreadMemManager),
                            MEM_COMMIT {$ifdef AlwaysAllocateTopDown} or MEM_TOP_DOWN{$endif},
                            PAGE_READWRITE);
    Result.Init;

    ThreadLock;
    if FFirstThreadMemory = nil then
      FFirstThreadMemory := Result
    else
    begin
      Result.FNextThreadManager := FFirstThreadMemory;
      FFirstThreadMemory := Result;
    end;
    ThreadUnLock;
  end;
end;

procedure TGlobalMemManager.IncSmallInterthreadMem;
begin
  Inc(FSmallInterThreadMemCount);
end;

procedure TGlobalMemManager.Init;
begin
  FGlobalThreadMemory := VirtualAlloc( nil,
                          SizeOf(TThreadMemManager),
                          MEM_COMMIT {$ifdef AlwaysAllocateTopDown} or MEM_TOP_DOWN{$endif},
                          PAGE_READWRITE);
  FGlobalThreadMemory.Init;
  FGlobalThreadMemory.FThreadId := 1;

  BGThread := TScaleMMBackGroundThread.Create;
end;

procedure TGlobalMemManager.ProcessFreedMemoryFromOtherThreads;
var
  tm1: PThreadMemManager;
//  mm, nextmm: PMediumBlockMemory;
begin
  //Exit;
  if not FGlobalThreadMemory.IsMemoryFromOtherThreadsPresent and
     not (FSmallInterThreadMemCount > 0) then Exit;

  //LOCK: no threads may be removed/freed now
  ThreadLock;
  try
    //in the mean time some inuse memory can be freed in an other thread
    FGlobalThreadMemory.ProcessFreedMemFromOtherThreads(False);

    if FSmallInterThreadMemCount > 0 then
    begin
      FSmallInterThreadMemCount := 0;

      tm1 := FFirstFreedThreadMemory;
      while tm1 <> nil do
      begin
        if tm1.IsMemoryFromOtherThreadsPresent then
          tm1.ProcessFreedMemFromOtherThreads(False);
        tm1 := tm1.FNextFreeThreadManager;
      end;
    end;

    (*  too much overhead to do this everytime!
    FGlobalThreadMemory.FMediumMemManager.ReleaseAllFreeMem;

    mm := Self.FFirstBlock;
    while mm <> nil do
    begin
      nextmm := mm.NextBlock;
      if mm.GetMaxFreeSizeInBlock = C_MAX_MEDIUMMEM_SIZE then
      begin
        if mm.PreviousBlock <> nil then
          mm.PreviousBlock.NextBlock := mm.NextBlock;
        if mm.NextBlock <> nil then
          mm.NextBlock.PreviousBlock := mm.PreviousBlock;

        if Self.FFirstBlock = mm then
        begin
          Self.FFirstBlock := nextmm;
          if nextmm <> nil then
            nextmm.PreviousBlock := nil;
        end;
        //RELEASE TO WINDOWS
        VirtualFree(mm, 0 {all}, MEM_RELEASE);
      end;
      mm := nextmm;
    end;
    *)
  finally
    //UNLOCK
    ThreadUnLock;
  end;
end;

procedure TGlobalMemManager.FreeBackGroundThread;
begin
  // 高凉新农+
  if Assigned(BGThread) then
  begin
    BGThread.Free;
    BGThread := nil;
  end;
end;

procedure TGlobalMemManager.StopBackGroundThread;
begin
  // 高凉新农+
  if Assigned(BGThread) then
    BGThread.StopThread;
end;

procedure TGlobalMemManager.ThreadLock;
var
  iCurrentThreadId: NativeUInt;
begin
  iCurrentThreadId := GetCurrentThreadId;
  if (FThreadLock = iCurrentThreadId) and
     (FThreadLockRecursion > 0) then
  begin
    Assert( CAS32(iCurrentThreadId, iCurrentThreadId, @FThreadLock) );
    inc(FThreadLockRecursion);
    Exit;
  end;

  //LOCK: no threads may be removed/freed now
  while not CAS32(0, iCurrentThreadId, @FThreadLock) do
  begin
    //small wait: try to swith to other pending thread (if any) else direct continue
    if not SwitchToThread then
      sleep(0);
    //try again
    if CAS32(0, iCurrentThreadId, @FThreadLock) then
      Break;
    //wait some longer: force swith to any other thread
    sleep(1);
  end;

  inc(FThreadLockRecursion);
end;

procedure TGlobalMemManager.ThreadUnLock;
//var
//  iCurrentThreadId: NativeUInt;
begin
  dec(FThreadLockRecursion);

  if FThreadLockRecursion = 0 then
  begin
    //iCurrentThreadId := GetCurrentThreadId;
    //UNLOCK
    //if not CAS32(iCurrentThreadId, 0, @FThreadLock) then
    //  Assert(False);
    FThreadLock := 0;
  end;
end;

function TGlobalMemManager.TryThreadLock: boolean;
var
  iCurrentThreadId: NativeUInt;
begin
  iCurrentThreadId := GetCurrentThreadId;
  if (FThreadLock = iCurrentThreadId) and
     (FThreadLockRecursion > 0) then
  begin
    Assert( CAS32(iCurrentThreadId, iCurrentThreadId, @FThreadLock) );
    inc(FThreadLockRecursion);
    Result := True;
    Exit;
  end;

  //LOCK: no threads may be removed/freed now
  Result := CAS32(0, iCurrentThreadId, @FThreadLock);
  if Result then
    inc(FThreadLockRecursion);
end;

{ TScaleMMBackGroundThread }

threadvar
  _GarbageCollectionActive: boolean;

var
  OldDLLProc: TDLLProc;

function ThreadProc(const aThread: TScaleMMBackGroundThread): Integer;
begin
  aThread.Execute;
  Result := 0;
  EndThread(Result);
end;

constructor TScaleMMBackGroundThread.Create;
//const
//  CREATE_SUSPENDED  = $00000004;
var
  iThreadID: Cardinal;
begin
  FHandle := BeginThread(nil, 0, @ThreadProc, Pointer(Self), 0 {direct start}, iThreadID);
end;

destructor TScaleMMBackGroundThread.Destroy;
begin
  // 高凉新农改
  inherited;
  if not FFinished then  // FFinished 一直为 False
    ExitThread(FHandle);
end;

procedure TScaleMMBackGroundThread.Execute;
var
  threadmm: PThreadMemManager;
  //iOldThreadId: Cardinal;
  i: Integer;
begin
  repeat
    //wait 5s
    for i := 1 to 50 do
    begin
      if FTerminated then
        Break;
      Sleep(100);
      //Sleep(5000);
    end;

    if not FTerminated then
    begin
      GlobalManager.ThreadLock;
      _GarbageCollectionActive := True;
      try
        threadmm := GlobalManager.GetFirstThreadMemory;
        while threadmm <> nil do
        begin
          //thread is terminated or killed? at least our ScaleMM2.NewEndThread function is not called! (note: is disabled because of this function :) )
          if WaitForSingleObject(threadmm.FThreadHandle, 0) = WAIT_OBJECT_0 then
          begin
            CloseHandle(threadmm.FThreadHandle);
            threadmm.FThreadHandle := 0;
            threadmm.FThreadId     := 0;
            threadmm.ProcessFreedMemFromOtherThreads(False {everything});
            GlobalManager.FreeThreadManager(threadmm);
          end;

          //interthread mem but not busy?
          if threadmm.IsMemoryFromOtherThreadsPresent and
             not threadmm.IsBusyLocked and
             (threadmm.FThreadHandle <> Self.FHandle) then     //do NOT suspend ourselves!
          begin
            //suspend thread
            if (threadmm.FThreadHandle <> 0) and (Integer(SuspendThread(threadmm.FThreadHandle)) >= 0) then
            //  Error(reAssertionFailed);   no error, can cause deadlocks  //error
            try
              //not busy in the meantime?
              if not threadmm.IsBusyLocked then
              begin
                threadmm.FastBusyLock;
                try
                  //iOldThreadId := threadmm.FThreadId;
                  //threadmm.FThreadId := 1;
                  threadmm.ProcessFreedMemFromOtherThreads(False {everything});
                finally
                  //threadmm.FThreadId := iOldThreadId;
                  threadmm.BusyUnLock;
                end;
              end;
            finally
              //resume
              ResumeThread(threadmm.FThreadHandle);
            end;
          end;

          threadmm := threadmm.FNextThreadManager;
        end;
      finally
        _GarbageCollectionActive := False;
        GlobalManager.ThreadUnLock;
      end;
    end;

  until FTerminated;
  FFinished := TRUE;
  FHandle := 0;  
end;

class function TScaleMMBackGroundThread.GarbageCollectionActive: boolean;
begin
  Result := _GarbageCollectionActive;
end;

procedure TScaleMMBackGroundThread.StopThread;
var
  i: Integer;
begin
  // 当 Exe 调用 DLL 时，如果 DLL 使用 ScaleMM
  // 此时线程无反应（可能停了，Delphi 有 Bug？），最多尝试 3 次
  if FHandle <> 0 then
  begin
    i := 0;
    FTerminated := TRUE;
    while not FFinished and (i < 3) do
    begin
      WaitForSingleObject(FHandle, 1 * 1000);
      Inc(i);
    end;
  end;
end;

procedure SMDLLMain(dwReason: Integer);
begin
  // 高凉新农改
  if Assigned(OldDLLProc) then
    OldDLLProc(dwReason);
end;

initialization
  OldDLLProc := DLLProc;
  DLLProc := @SMDLLMain;

finalization
  DLLProc := OldDLLProc;
{  if BGThread <> nil then   // 高凉新农-
  begin
    BGThread.Free;
    BGThread := nil;
  end;        }

end.

