-- lazy.nvim
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not (vim.uv or vim.loop).fs_stat(lazypath) then
  vim.fn.system({
    "git",
    "clone",
    "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable", -- latest stable release
    lazypath,
  })
end
vim.opt.rtp:prepend(lazypath)

require("lazy").setup({
  {
    "folke/tokyonight.nvim",
    lazy = false,
    priority = 1000,
    opts = {},
  },
  {
    "lmgraf/wsl-clipboard.nvim",
    opts = {
      mode = "sync", -- options: "system", "sync", "focus"
    },
  },
  {
    "mason-org/mason.nvim",
    build = ":MasonUpdate",
    cmd = { "Mason", "MasonUpdate", "MasonLog", "MasonInstall", "MasonUninstall", "MasonUninstallAll" },
    config = true,
  },
  {
    "mason-org/mason-lspconfig.nvim",
    opts = {
	ensure_installed = {
	    "pyright",
        "gopls",
	}
    },
    dependencies = {
      { "mason-org/mason.nvim" },
      { "neovim/nvim-lspconfig" },
    },
    event = { "BufReadPre", "BufNewFile" },
    config = true,
    keys = {
      { "<C-q>", "<cmd>lua vim.lsp.completion.get()  <CR>", mode = "i" },
      { "gh",        "<cmd>lua vim.lsp.buf.hover()       <CR>" },
      { "gd",        "<cmd>lua vim.lsp.buf.definition()  <CR>" },
      { "gr",        "<cmd>lua vim.lsp.buf.references() <CR>" },
      { "gR",        "<cmd>lua vim.lsp.buf.rename() <CR>" },
    },
  },
  {
    'nvim-telescope/telescope.nvim', 
    dependencies = {
        'nvim-lua/plenary.nvim',
        -- optional but recommended
        { 'nvim-telescope/telescope-fzf-native.nvim', build = 'make' },
    },
    keys = {
      { mode = "n", "<Leader>ff", "<cmd>Telescope find_files<CR>", {} },
      { mode = "n", "<Leader>fg", "<cmd>Telescope live_grep<CR>", {} },
      { mode = "n", "<Leader>fb", "<cmd>Telescope buffers<CR>", {} },
      { mode = "n", "<Leader>fh", "<cmd>Telescope help_tags<CR>", {} },
    },
    opts = function()
      local actions = require('telescope.actions')
      return {
      defaults = {
          layout_config = {
              width = 0.75,
          },
          file_ignore_patterns = {
              "%.git/",
              "%.metadata/",
              "%.venv/",
          },
          mappings = {
              i = {
                ["<C-q>"] = require('telescope.actions').send_to_qflist 
                + require('telescope.actions').open_qflist,
              },
              n = {
                ["<C-q>"] = require('telescope.actions').send_to_qflist 
                + require('telescope.actions').open_qflist,
              },
          }
      },
      pickers = {
          find_files = {
              hidden = true,
          },
      },
    }
    end,
  },
  {'akinsho/toggleterm.nvim', version = "*", config = true}
})

-- Encoding - only when modifiable is on
local group = vim.api.nvim_create_augroup("ModifiableOnlySettings", { clear = true })

vim.api.nvim_create_autocmd({ "BufEnter", "BufWinEnter" }, {
  group = group,
  callback = function(args)
    local is_modifiable = vim.api.nvim_get_option_value("modifiable", { buf = args.buf })

    if is_modifiable then
      vim.opt.fileencoding = "utf-8"
      vim.opt.fileencodings = { "utf-8", "sjis", "iso-2022-jp", "euc-jp" }
      vim.opt.fileformats = { "unix", "dos" }
    end
  end,
})

-- Edit/Input
vim.opt.autoread = true

vim.opt.autoindent = true
vim.opt.smartindent = true

vim.opt.expandtab = true
vim.opt.smarttab = true
vim.opt.shiftwidth = 4
vim.opt.tabstop = 4

vim.opt.whichwrap = "b,s,h,l,<,>,[,]"

-- Search
vim.opt.hlsearch = true
vim.opt.incsearch = true

vim.opt.ignorecase = true
vim.opt.smartcase = true

vim.opt.wrapscan = true

-- Appearance
vim.opt.cursorline = true
vim.opt.cursorcolumn = true
vim.opt.visualbell = true

vim.opt.list = true
vim.opt.listchars = { eol = "$", tab = "> ", extends = "<" }
-- vim.opt.showtabline = 2

vim.opt.number = true
vim.cmd("syntax on")
vim.opt.title = true
vim.opt.showmatch = true
vim.opt.matchtime = 1
vim.opt.ruler = true
vim.opt.statusline = "%<%f %m%r%h%w%{'['.(&fenc!=''?&fenc:&enc).'/'.&ff.']'}%=%l,%c%V%6P"
vim.opt.linespace = 0
vim.opt.showcmd = true

---- Color scheme
vim.cmd.colorscheme("tokyonight-storm")

-- Clipboard (WSL)
-- https://scrapbox.io/takker/WSL2%E3%81%AEneovim%E3%81%A7clipboard%E3%82%92%E4%BD%BF%E3%81%86
--if vim.fn.system('uname -a | grep microsoft') ~= '' then
--    vim.g.clipboard = {
--        name = "WslClipboard",
--        copy = {
--            ["+"] = "clip.exe",
--            ["*"] = "clip.exe",
--        },
--        cache_enabled=1,
--    }
--end


-- Lazygit+toggleterm
local Terminal = require("toggleterm.terminal").Terminal
local lazygit = Terminal:new({
    cmd = "lazygit",
    direction = "float",
    hidden = true
})

function _lazygit_toggle()
    lazygit:toggle()
end

-- Generic window function
local function _open_cmd_in_window(cmd)
    -- window config
    local stats = vim.api.nvim_list_uis()[1]
    local width = math.floor(stats.width * 0.8)
    local height = math.floor(stats.height * 0.8)
    local col = math.floor((stats.width - width) / 2)
    local row = math.floor((stats.height - height) / 2)

    local buf = vim.api.nvim_create_buf(false, true)

    local win = vim.api.nvim_open_win(buf, true, {
        relative = "editor",
        width = width,
        height = height,
        row = row,
        col = col,
        style = "minimal",
        border = "rounded",
    })

    vim.fn.termopen(cmd)

    local opts = { buffer = buf, silent = true }
    vim.keymap.set("n", "q", "<cmd>close<cr>", opts)
    vim.keymap.set("t", "<Esc>", [[<C-\><C-n><cmd>close<cr>]], opts)
end


-- Git quick blame

local function _quick_git_blame()
    local filepath = vim.api.nvim_buf_get_name(0)
    if filename == "" then
      vim.notify("Error: no filename in the buffer.", vim.log.levels.WARN)
      return
    end

    local line = vim.api.nvim_win_get_cursor(0)[1]
    local cmd = "git --no-pager show $(git blame -L %d,%d -s %s | awk '{print $1}' | tr -d '^')"
    local full_cmd = string.format(cmd, line, line, vim.fn.shellescape(filepath))

     _open_cmd_in_window(full_cmd)
end

vim.api.nvim_create_user_command("QuickGitBlame", function(opts)
  _quick_git_blame(opts.args)
end, {
})

-- Function
local appearance_status = 1

local function toggle_appearance()
  if appearance_status == 0 then
    vim.opt.listchars = { eol = "$", tab = "> ", extends = "<" }
    vim.opt.number = true
    vim.cmd("syntax on")
  else
    vim.opt.listchars = {}
    vim.opt.number = false
    vim.cmd("syntax off")
  end

  appearance_status = 1 - appearance_status
end

-- Keybind
vim.keymap.set("n", ";;", toggle_appearance, { noremap = true, silent = true, desc = "Toggle appearance" })
vim.keymap.set("n", "<Esc><Esc>", "<cmd>nohlsearch<CR><Esc>", { noremap = true, silent = true})
vim.api.nvim_set_keymap("n", "<Leader>lg", "<cmd>lua _lazygit_toggle()<CR>", { noremap = true, silent = true})

require("device")
