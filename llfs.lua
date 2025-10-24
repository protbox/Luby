local llfs = {}

local ffi = require("ffi")
local is_windows = ffi.os == "Windows"

if not is_windows then
    -- Unix/POSIX
    ffi.cdef[[
        typedef struct DIR DIR;
        
        struct dirent {
            unsigned long d_ino;
            unsigned long d_off;
            unsigned short d_reclen;
            unsigned char d_type;
            char d_name[256];
        };
        
        DIR* opendir(const char* name);
        struct dirent* readdir(DIR* dirp);
        int closedir(DIR* dirp);
        int mkdir(const char* pathname, unsigned int mode);
        
        static const int DT_DIR = 4;
    ]]
    
    function llfs.is_dir(path)
        local d = ffi.C.opendir(path)
        if d == nil then
            return false
        end
        ffi.C.closedir(d)
        return true
    end
    
    function llfs.mkdir(path)
        ffi.C.mkdir(path, tonumber("755", 8))
    end
    
    function llfs.dir(path)
        local d = ffi.C.opendir(path)
        if d == nil then
            return {}
        end
        
        local files = {}
        while true do
            local entry = ffi.C.readdir(d)
            if entry == nil then break end
            
            local name = ffi.string(entry.d_name)
            if name ~= "." and name ~= ".." then
                table.insert(files, name)
            end
        end
        
        ffi.C.closedir(d)
        return files
    end
else
    -- Winblows (UNTESTED)
    ffi.cdef[[
        typedef struct _WIN32_FIND_DATAA {
            uint32_t dwFileAttributes;
            char cFileName[260];
        } WIN32_FIND_DATAA;
        
        void* FindFirstFileA(const char* lpFileName, WIN32_FIND_DATAA* lpFindFileData);
        int FindNextFileA(void* hFindFile, WIN32_FIND_DATAA* lpFindFileData);
        int FindClose(void* hFindFile);
        int GetFileAttributesA(const char* lpFileName);
        int CreateDirectoryA(const char* lpPathName, void* lpSecurityAttributes);
        
        static const int FILE_ATTRIBUTE_DIRECTORY = 0x10;
        static const int INVALID_FILE_ATTRIBUTES = -1;
    ]]
    
    function llfs.is_dir(path)
        local attrs = ffi.C.GetFileAttributesA(path)
        if attrs == ffi.C.INVALID_FILE_ATTRIBUTES then
            return false
        end
        return bit.band(attrs, ffi.C.FILE_ATTRIBUTE_DIRECTORY) ~= 0
    end
    
    function llfs.mkdir(path)
        ffi.C.CreateDirectoryA(path, nil)
    end
    
    function llfs.dir(path)
        local search_path = path .. "\\*"
        local find_data = ffi.new("WIN32_FIND_DATAA")
        local handle = ffi.C.FindFirstFileA(search_path, find_data)
        
        if handle == ffi.cast("void*", -1) then
            return {}
        end
        
        local files = {}
        repeat
            local name = ffi.string(find_data.cFileName)
            if name ~= "." and name ~= ".." then
                table.insert(files, name)
            end
        until ffi.C.FindNextFileA(handle, find_data) == 0
        
        ffi.C.FindClose(handle)
        return files
    end
end

function llfs.path_exists(path)
    local f = io.open(path, "r")
    if f then
        f:close()
        return true
    end
    return llfs.is_dir(path)
end

return llfs