namespace Hearts.Learn

open System
open System.IO
open System.IO.MemoryMappedFiles

/// Memory-mapped persistent storage for training samples.
/// All samples are kept forever; file grows dynamically as needed.
type PersistentReservoir =
    private {
        FilePath : string
        mutable MappedFile : MemoryMappedFile
        mutable Accessor : MemoryMappedViewAccessor
        mutable AllocatedCapacity : int
        mutable Count : int
        BaseSeed : int
    }

    interface IDisposable with
        member this.Dispose() =
            this.Accessor.Dispose()
            this.MappedFile.Dispose()

module PersistentReservoir =

    /// Magic bytes identifying the file format.
    let private magic = "HRTSRESV"B

    /// File format version.
    let private version = 2u

    /// Header size in bytes.
    let private headerSize = 64L

    /// Offset of allocated capacity field in header.
    let private allocatedCapacityOffset = 12L

    /// Offset of count field in header.
    let private countOffset = 16L

    /// Offset of baseSeed field in header.
    let private baseSeedOffset = 20L

    /// Initial capacity when creating a new file.
    let private initialCapacity = 1_000_000

    /// Calculates file offset for a record at the given index.
    let private recordOffset index =
        headerSize + int64 index * int64 SampleSerializer.recordSize

    /// Calculates required file size for given capacity.
    let private fileSize capacity =
        recordOffset capacity

    /// Writes the file header.
    let private writeHeader (accessor : MemoryMappedViewAccessor) (allocatedCapacity : int) (count : int) (baseSeed : int) =
        accessor.WriteArray(0L, magic, 0, magic.Length)
        accessor.Write(8L, version)
        accessor.Write(allocatedCapacityOffset, uint32 allocatedCapacity)
        accessor.Write(countOffset, uint32 count)
        accessor.Write(baseSeedOffset, baseSeed)
        accessor.Flush()

    /// Updates the count field in header.
    let private updateCount (accessor : MemoryMappedViewAccessor) (count : int) =
        accessor.Write(countOffset, uint32 count)

    /// Updates the allocated capacity field in header.
    let private updateAllocatedCapacity (accessor : MemoryMappedViewAccessor) (allocatedCapacity : int) =
        accessor.Write(allocatedCapacityOffset, uint32 allocatedCapacity)

    /// Reads and validates the file header.
    let private readHeader (accessor : MemoryMappedViewAccessor) =
        let magicBytes = Array.zeroCreate magic.Length
        accessor.ReadArray(0L, magicBytes, 0, magic.Length) |> ignore
        if magicBytes <> magic then
            failwith "Invalid reservoir file: bad magic"

        let ver = accessor.ReadUInt32(8L)
        if ver <> version then
            failwith $"Unsupported reservoir version: {ver} (expected {version})"

        let allocatedCapacity = accessor.ReadUInt32(allocatedCapacityOffset) |> int
        let count = accessor.ReadUInt32(countOffset) |> int
        let baseSeed = accessor.ReadInt32(baseSeedOffset)

        allocatedCapacity, count, baseSeed

    /// Opens a memory-mapped file.
    let private openMappedFile filePath size =
        let mmf = MemoryMappedFile.CreateFromFile(
            filePath,
            FileMode.Open,
            null,
            size,
            MemoryMappedFileAccess.ReadWrite)
        let accessor = mmf.CreateViewAccessor()
        mmf, accessor

    /// Grows the file to accommodate more samples.
    let private grow (reservoir : PersistentReservoir) =
        // Close current mapping
        reservoir.Accessor.Dispose()
        reservoir.MappedFile.Dispose()

        // Double the capacity
        let newCapacity = reservoir.AllocatedCapacity * 2
        let newSize = fileSize newCapacity

        // Resize file
        use fs = new FileStream(reservoir.FilePath, FileMode.Open, FileAccess.ReadWrite, FileShare.None)
        fs.SetLength(newSize)

        // Reopen mapping
        let mmf, accessor = openMappedFile reservoir.FilePath newSize
        reservoir.MappedFile <- mmf
        reservoir.Accessor <- accessor
        reservoir.AllocatedCapacity <- newCapacity

        // Update header
        updateAllocatedCapacity accessor newCapacity

    /// Creates a new persistent reservoir.
    let create filePath baseSeed =
        let capacity = initialCapacity
        let size = fileSize capacity

        // Create file
        use fs = new FileStream(filePath, FileMode.Create, FileAccess.ReadWrite, FileShare.None)
        fs.SetLength(size)

        let mmf, accessor = openMappedFile filePath size

        writeHeader accessor capacity 0 baseSeed

        {
            FilePath = filePath
            MappedFile = mmf
            Accessor = accessor
            AllocatedCapacity = capacity
            Count = 0
            BaseSeed = baseSeed
        }

    /// Opens an existing persistent reservoir.
    let openExisting filePath =
        if not (File.Exists filePath) then
            failwith $"Reservoir file not found: {filePath}"

        let fileInfo = FileInfo(filePath)
        let mmf, accessor = openMappedFile filePath fileInfo.Length

        let allocatedCapacity, count, baseSeed = readHeader accessor

        {
            FilePath = filePath
            MappedFile = mmf
            Accessor = accessor
            AllocatedCapacity = allocatedCapacity
            Count = count
            BaseSeed = baseSeed
        }

    /// Gets the number of samples stored.
    let count reservoir = reservoir.Count

    /// Gets the total number of samples seen (same as count, kept for API compatibility).
    let numSeen reservoir = int64 reservoir.Count

    /// Gets the base seed.
    let baseSeed reservoir = reservoir.BaseSeed

    /// Adds a sample to storage.
    let add sample (reservoir : PersistentReservoir) =
        // Grow if needed
        if reservoir.Count >= reservoir.AllocatedCapacity then
            grow reservoir

        let idx = reservoir.Count
        reservoir.Count <- reservoir.Count + 1

        let offset = recordOffset idx
        SampleSerializer.writeTo reservoir.Accessor offset sample

    /// Adds multiple samples to storage.
    let addMany samples (reservoir : PersistentReservoir) =
        for sample in samples do
            add sample reservoir

    /// Flushes changes to disk.
    let flush (reservoir : PersistentReservoir) =
        updateCount reservoir.Accessor reservoir.Count
        reservoir.Accessor.Flush()

    /// Iterates over all samples.
    let items (reservoir : PersistentReservoir) =
        seq {
            for i = 0 to reservoir.Count - 1 do
                let offset = recordOffset i
                yield SampleSerializer.readFrom reservoir.Accessor offset
        }
