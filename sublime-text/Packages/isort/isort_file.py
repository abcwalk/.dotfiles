import os
import sys

import sublime
import sublime_plugin

from .isort import SortImports

sys.path.append(os.path.dirname(__file__))

DEFAULT_SORT_ON_SAVE = True


def is_python(view):
    return view.match_selector(0, "source.python")


class IsortCommand(sublime_plugin.TextCommand):
    def is_enabled(self):
        return is_python(self.view)

    is_visible = is_enabled

    def get_region(self):
        return sublime.Region(0, self.view.size())

    def get_buffer_contents(self):
        return self.view.substr(self.get_region())

    def set_cursor_back(self, begin_positions):
        for pos in begin_positions:
            self.view.sel().add(pos)

    def sort_imports(self, edit):
        current_positions = list(self.view.sel())

        this_contents = self.get_buffer_contents()
        settings = self.view.settings().get('isort') or {}
        sorted_imports = SortImports(
            file_path=self.view.file_name(),  # to load isort settings
            file_contents=this_contents,
            **settings
        ).output
        self.view.replace(edit, self.get_region(), sorted_imports)

        # Our sel has moved now..
        remove_sel = self.view.sel()[0]
        self.view.sel().subtract(remove_sel)
        self.set_cursor_back(current_positions)

    def run(self, edit):
        self.sort_imports(edit)


class IsortOnSave(sublime_plugin.ViewEventListener):
    def on_pre_save(self):
        settings = self.view.settings() or {}
        sort_on_save = settings.get('isort.sort_on_save')
        if sort_on_save is None:
            sort_on_save = DEFAULT_SORT_ON_SAVE
        if sort_on_save:
            self.view.run_command('isort')
