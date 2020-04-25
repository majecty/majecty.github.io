---
title: Gnome Tweak이 하는 일 추적하기
author: 주형
tags: gnome, python
---

## 목적

gnome-tweaks에서 "Additional Layout Options" 버튼을 누르면 나오는 창에서 설정한 키보드 옵션이 어떻게 적용되는지 분석하자.

## Gnome Tweaks

Gnome Tweaks는 Gnome 3의 고급 기능을 설정하는 GUI도구다.
OS의 설정 앱에서는 제공하지 않는 숨겨진 기능들을 설정할 수 있다.
키보드 옵션 중에는 CapsLock키의 동작을 Ctrl로 바꾼다거나 오른쪽 알트 키를 한영키로 동작시키는 옵션을 제공한다.
Keyboard & Mouse 탭에서 키보드와 마우스에 대한 옵션들을 설정할 수 있으며, 그 중  "Additional Layout Options" 버튼을 클릭하면 키보드의 조합키들에 관한 다양한 옵션을 바꿀 수 있다.

Gnome Tweaks의 소스코드는 Gitlab[^1]에서 받을 수 있다.
프로젝트의 루트 디렉토리에는 README.md파일과 빌드 관련 파일들이 있다.
소스코드는 gtweak 디렉토리 안에 있는데,  tweaks 디렉토리에는 UI에 대응되는 코드들이 있다.
tweak_group_font.py는 폰트 설정창을 담당하는 코드고, tweak_group_keymouse.py파일이 키보드 마우스 설정창을 담당한다.

[^1]: [Gnome Tweaks repository](https://gitlab.gnome.org/GNOME/gnome-tweaks)

## AdditionalLayoutButton

gtweak/tweaks/tweak_group_keymouse.py[^2] 파일을 열면 `AdditionalLayoutButton`[^3] class를 찾을 수 있다.
`AdditionalLayoutButton` class는 "Additional Layout Button"의 모양을 결정하고, 버튼이 클릭되었을 때의 동작을 명시한다.

```python
# Gtk.Box를 상속한다. Gtk는 UI를 커스터마이징하기 위해서 상속을 사용한다.
class AdditionalLayoutButton(Gtk.Box, Tweak):

    def __init__(self):
        Gtk.Box.__init__(self, orientation=Gtk.Orientation.VERTICAL, spacing=18,
                               valign=Gtk.Align.CENTER)
        Tweak.__init__(self, "extensions", "")

        # 버튼을 만든다.
        btn = Gtk.Button(label=_("Additional Layout Options"),halign=Gtk.Align.END)
        # 버튼이 눌렸을 때 이벤트를 등록한다.
        btn.connect("clicked", self._on_browse_clicked)
        # Box안에 버튼을 넣는다.
        self.add(btn)

        self.show_all()

```

이 버튼은 클릭[^4]했을 dialog를 만들고[^5], 그 안에 `ScrolledWindow`를 만들고[^6], 다시 그 안에 `TypingTweakGroup`[^7]을 만든다.

```python

def _on_browse_clicked(self, btn):
    # dialog를 만든다.
    dialog = Gtk.Window()
    dialog.set_title(_("Additional Layout Options"))
    dialog.set_type_hint(Gdk.WindowTypeHint.DIALOG)
    dialog.set_transient_for(self.main_window)
    dialog.set_modal(True)

    dialog.set_size_request(500,500)
    geometry = Gdk.Geometry()
    geometry.max_height = 500
    dialog.set_geometry_hints(None, geometry, Gdk.WindowHints.MAX_SIZE)

    # ScrolledWindow를 만든다.
    scrolled_window = Gtk.ScrolledWindow()
    scrolled_window.set_border_width(10)
    # ScrolledWindow안에 TypingTweakGroup을 만든다.
    box = TypingTweakGroup()
    scrolled_window.add_with_viewport(box)

    dialog.add(scrolled_window)
    dialog.show_all()

```

[^2]: [gtweak/tweaks/tweak_group_keymouse.py](https://gitlab.gnome.org/GNOME/gnome-tweaks/-/blob/ebc0f25d361d172385302b9c9ba12503571a11cf/gtweak/tweaks/tweak_group_keymouse.py)
[^3]: [gtweak/tweaks/tweak_group_keymouse.py#L216](https://gitlab.gnome.org/GNOME/gnome-tweaks/-/blob/ebc0f25d361d172385302b9c9ba12503571a11cf/gtweak/tweaks/tweak_group_keymouse.py#L216)
[^4]: [gtweak/tweaks/tweak_group_keymouse.py#L229](https://gitlab.gnome.org/GNOME/gnome-tweaks/-/blob/ebc0f25d361d172385302b9c9ba12503571a11cf/gtweak/tweaks/tweak_group_keymouse.py#L229)
[^5]: [gtweak/tweaks/tweak_group_keymouse.py#L229](https://gitlab.gnome.org/GNOME/gnome-tweaks/-/blob/ebc0f25d361d172385302b9c9ba12503571a11cf/gtweak/tweaks/tweak_group_keymouse.py#L229)
[^6]: [gtweak/tweaks/tweak_group_keymouse.py#L241](https://gitlab.gnome.org/GNOME/gnome-tweaks/-/blob/ebc0f25d361d172385302b9c9ba12503571a11cf/gtweak/tweaks/tweak_group_keymouse.py#L241)
[^7]: [gtweak/tweaks/tweak_group_keymouse.py#L241](https://gitlab.gnome.org/GNOME/gnome-tweaks/-/blob/ebc0f25d361d172385302b9c9ba12503571a11cf/gtweak/tweaks/tweak_group_keymouse.py#L241)

## TypingTweakGroup

Additional Layout Options을 눌렀을 때 뜨는 창에는 옵션의 그룹들이 들어 있으며, 그룹을 열면 그룹 안에 체크박스 혹은 라디오 박스 여럿으로 구성되어 있다.
예를 들어 "캡스락 키의 동작" 그룹을 열면 "끄기", "Ctrl로 사용", "Alt로 사용" 등의 옵션들이 표시된다.

gtweak/tweaks/tweak_group_xkb.py[^8] 파일을 열면 `TypingTweakGroup`[^9] 클래스가 정의되어 있다.
이 클래스는 `GnomeDesktop.XKBInfo.get_all_options_groups`함수를 호출해[^10] 옵션들을 읽고 각 옵션마다 _XkbOption UI를 만들어 `self.pack_start(option, ...)`을 호출해 화면에 옵션을 표시한다.

```python
class TypingTweakGroup(Gtk.Box):
    # ...
    def __init__(self):
        Gtk.Box.__init__(self, orientation=Gtk.Orientation.VERTICAL, spacing=3)
        self._option_objects = []
        ok = False
        try:
            # ...
        except GSettingsMissingError:
            # ...
        except AttributeError:
            # ...
        finally:
            if ok:
                # 모든 옵션을 돌면서
                for opt in set(self._xkb_info.get_all_option_groups()) - self.XKB_OPTIONS_BLACKLIST:
                    # _XkbOption 인스턴스를 만들고,
                    obj = _XkbOption(opt, self._kbdsettings, self._xkb_info)
                    self._option_objects.append(obj)
                    # 인스턴스를 self(box)에 넣는다.
                    self.pack_start(obj, False, False, 0)
        # ...
```


[^8]: [gtweak/tweaks/tweak_group_xkb.py](https://gitlab.gnome.org/GNOME/gnome-tweaks/-/blob/ebc0f25d361d172385302b9c9ba12503571a11cf/gtweak/tweaks/tweak_group_xkb.py)
[^9]: [gtweak/tweaks/tweak_group_xkb.py#L137](https://gitlab.gnome.org/GNOME/gnome-tweaks/-/blob/ebc0f25d361d172385302b9c9ba12503571a11cf/gtweak/tweaks/tweak_group_xkb.py#L137)
[^10]: [gtweak/tweaks/tweak_group_xkb.py#L162](https://gitlab.gnome.org/GNOME/gnome-tweaks/-/blob/ebc0f25d361d172385302b9c9ba12503571a11cf/gtweak/tweaks/tweak_group_xkb.py#L162)

## _XkbOption

`_XkbOption` class[^11]는 `TypingTweakGroup`과 같은 파일에 선언되어있다. 
`_XkbOption`은 GTK의 `Expander` 클래스를 상속하고 있으며, 각 옵션 그룹 안의 옵션값 읽어온 다음[^12] 옵션별로 체크버튼[^13]이나 라디오 버튼[^14]을 만든다.
각 옵션은 토글되었을 때 `_XkbOption`의 `_on_toggled` 함수를 호출한다[^15].
_on_toggled 함수[^16]는 상태에 따라서 `self._parent_settings`의 `setting_remove_from_list`, `setting_add_to_list` 중 하나를 호출한다.

```python
# Gtk.Expander를 상속한다.
class _XkbOption(Gtk.Expander, Tweak):
    def __init__(self, group_id, parent_settings, xkb_info, **options):
        # ...
        Gtk.Expander.__init__(self)
        Tweak.__init__(self, desc, desc, **options)

        self.set_label(self.name)
        vbox = Gtk.Box(orientation=Gtk.Orientation.VERTICAL, spacing=3)
        vbox.set_margin_start(15)
        self.add(vbox)

        # ...

        model_values = []

        # ...

        # group에 있는 모든 옵션을 돌면서
        for option_id in self._xkb_info.get_options_for_group(group_id):
            desc = self._xkb_info.description_for_option(group_id, option_id)
            # model_values 변수를 채운다.
            model_values.append((option_id, desc))
            self._possible_values.append(option_id)

        # ...

        model_values.sort(key=values_cmp_py3_wrap(values_cmp))

        self._widgets = dict()
        for (val, name) in model_values:
            w = None
            # 옵션의 특징에 따라서
            if self._multiple_selection:
                # 체크박스를 만들거나
                w = Gtk.CheckButton.new()
            else:
                # 라디오버튼을 만든다.
                w = Gtk.RadioButton.new_from_widget(self._widgets.get(None))
            self._widgets[val] = w;
            vbox.add(w)
            l = Gtk.Label(label=name)
            l.set_line_wrap(True)
            w.add(l)
            # 옵션이 수정되었을 때 self._on_toggled가 호출된다.
            w._changed_id = w.connect('toggled', self._on_toggled)
            w._val = val

        self.widget_for_size_group = None
        self.reload()


    # 옵션이 변경되었을 때 호출된다.
    def _on_toggled(self, w):
        active = w.get_active()
        if not self._multiple_selection and active:
            for v in self._values:
                # 옵션을 리스트에서 없앤다.
                self._parent_settings.setting_remove_from_list(TypingTweakGroup.XKB_GSETTINGS_NAME, v)

        if w._val in self._values and not active:
            # 옵션을 리스트에서 없앤다.
            self._parent_settings.setting_remove_from_list(TypingTweakGroup.XKB_GSETTINGS_NAME, w._val)
        elif active and not w._val in self._values and w._val:
            # 옵션을 리스트에 추가한다.
            self._parent_settings.setting_add_to_list(TypingTweakGroup.XKB_GSETTINGS_NAME, w._val)

```

[^11]: [gtweak/tweaks/tweak_group_xkb.py#17](https://gitlab.gnome.org/GNOME/gnome-tweaks/-/blob/ebc0f25d361d172385302b9c9ba12503571a11cf/gtweak/tweaks/tweak_group_xkb.py#L17)
[^12]: [gtweak/tweaks/tweak_group_xkb.py#L43](https://gitlab.gnome.org/GNOME/gnome-tweaks/-/blob/ebc0f25d361d172385302b9c9ba12503571a11cf/gtweak/tweaks/tweak_group_xkb.py#L43)
[^13]: [gtweak/tweaks/tweak_group_xkb.py#L80](https://gitlab.gnome.org/GNOME/gnome-tweaks/-/blob/ebc0f25d361d172385302b9c9ba12503571a11cf/gtweak/tweaks/tweak_group_xkb.py#L80)
[^14]: [gtweak/tweaks/tweak_group_xkb.py#L82](https://gitlab.gnome.org/GNOME/gnome-tweaks/-/blob/ebc0f25d361d172385302b9c9ba12503571a11cf/gtweak/tweaks/tweak_group_xkb.py#L82)
[^15]: [gtweak/tweaks/tweak_group_xkb.py#L88](https://gitlab.gnome.org/GNOME/gnome-tweaks/-/blob/ebc0f25d361d172385302b9c9ba12503571a11cf/gtweak/tweaks/tweak_group_xkb.py#L88)
[^16]: [gtweak/tweaks/tweak_group_xkb.py#L126](https://gitlab.gnome.org/GNOME/gnome-tweaks/-/blob/ebc0f25d361d172385302b9c9ba12503571a11cf/gtweak/tweaks/tweak_group_xkb.py#L126)

## GSettingsSetting

`setting_add_to_list` 함수 `GsettingsSetting` 클래스의 메소드로 gtweak/gsettings.py[^17] 파일에 선언되어있다.
GSettings가 간단한 값을 저장하고 읽는 것만을 지원하기 때문에 리스트를 쉽게 다룰 수 있도록
값을 하나 추가 혹은 삭제하는 함수들이다.
`setting_add_to_list` 함수는 `self[key] = vals`[^18] 코드로 설정 값을 저장한다. Python에서 `[]` 연산자로 값을 쓰는 경우 `__setitem__` 함수가 호출된다.

```python
    def setting_add_to_list(self, key, value):
        """ helper function, ensures value is present in the GSettingsList at key """
        assert self._setting_check_is_list(key)

        vals = self[key]
        if value not in vals:
            vals.append(value)
            self[key] = vals
            return True

    def setting_remove_from_list(self, key, value):
        """ helper function, removes value in the GSettingsList at key (if present)"""
        assert self._setting_check_is_list(key)

        vals = self[key]
        try:
            vals.remove(value)
            self[key] = vals
            return True
        except ValueError:
            # not present
            pass

```

[^17]: [gtweak/gsettings.py#L161](https://gitlab.gnome.org/GNOME/gnome-tweaks/-/blob/ebc0f25d361d172385302b9c9ba12503571a11cf/gtweak/gsettings.py#L161)
[^18]: [gtweak/gsettings.py#L178](https://gitlab.gnome.org/GNOME/gnome-tweaks/-/blob/ebc0f25d361d172385302b9c9ba12503571a11cf/gtweak/gsettings.py#L178)

## PyGObject의 GSettings

`__setitem__` 함수는 `GsettingsSetting`가 상속하고 있는 `Gio.Settings` 클래스에 정의되어있다. `Gio` 코드는 PyGObject[^19] 프로젝트에 정의되어 있다. PyGObject는 Python언어에서 Gnome과 관련된 GTK, GStreamer, WebKitGTK, GLib, GIO등의 라이브러리에 대한 바인딩을 제공해주는 라이브러리다.
PyGObject레포지토리의 gi/overrides/Gio.py[^20] 코드 안에 `Settings`[^21] class가 선언되어있으며, 그 안에 `__setitem__`[^22] 함수가 선언되어있다. `Settings` class는 gio의 Settings 모듈에 Python 딕셔너리 인터페이스를 제공해주는 클래스다.
`__setitem__`함수는 `set_value`[^23] 함수를 호출하는데, 이 함수는 `gio`의 `g_settings_set_value`[^24]의 wrapper다.[^wrapper]

```python
class Settings(Gio.Settings):
    '''Provide dictionary-like access to GLib.Settings.'''

    __init__ = deprecated_init(Gio.Settings.__init__,
                               arg_names=('schema', 'path', 'backend'))

    def __contains__(self, key):
        return key in self.list_keys()

    def __len__(self):
        return len(self.list_keys())

    def __iter__(self):
        for key in self.list_keys():
            yield key

    # ...

    def __setitem__(self, key, value):
        # set_value() aborts the program on an unknown key
        if key not in self:
            raise KeyError('unknown key: %r' % (key,))

        # ...
        if type_ == 'type':
            # ...
        elif type_ == 'enum':
            # ...
        elif type_ == 'range':
            # ...
        else:
            raise NotImplementedError('Cannot handle allowed type range class ' + str(type_))

        # C로 짜여진 gio의 g_settings_set_value 함수를 호출한다.
        self.set_value(key, GLib.Variant(type_str, value))
```

[^19]: [PyGObject repository](https://gitlab.gnome.org/GNOME/pygobject)
[^20]: [gi/overrides/Gio.py](https://gitlab.gnome.org/GNOME/pygobject/-/blob/1a2bc1d0806ab6178f65125bf0b2283eb3378d4d/gi/overrides/Gio.py)
[^21]: [gi/overrides/Gio.py#L233](https://gitlab.gnome.org/GNOME/pygobject/-/blob/1a2bc1d0806ab6178f65125bf0b2283eb3378d4d/gi/overrides/Gio.py#L233)
[^22]: [gi/overrides/Gio.py#L264](https://gitlab.gnome.org/GNOME/pygobject/-/blob/1a2bc1d0806ab6178f65125bf0b2283eb3378d4d/gi/overrides/Gio.py#L264)
[^23]: [gi/overrides/Gio.py#L295](https://gitlab.gnome.org/GNOME/pygobject/-/blob/1a2bc1d0806ab6178f65125bf0b2283eb3378d4d/gi/overrides/Gio.py#L295)
[^24]: [gi의 g_settings_set_value](https://developer.gnome.org/gio/stable/GSettings.html#g-settings-set-value)
[^wrapper]:
  Gnome 프로젝터는 C로 짜여진 그놈 코드로부터 메타데이터를 추출하여 다른 언어들이 바인딩을 생성하는 전략을 쓴다.
  Python은 import 구문을 실행할 때 메타데이터를 읽어서 wrapper 함수나 모듈을 생성한다.

## GObject introspection

GTK는 다양한 언어에 대한 binding을 쉽게 만들기 위해서 특별한 구조를 사용한다.
GObject Introspection 프로젝트[^gobject-introspection]는 GTK 소스코드로부터 각 API정보를 XML이나 바이너리 파일로 추출해서 해당 정보를 바탕으로 binding을 생성한다. Python binding은 Python 소스코드에서 import하는 과정에서 API 정보를 바탕으로 바인딩 코드를 생성한다.

[^gobject-introspection]: [GObject Introspection](https://gi.readthedocs.io/en/latest/)

다음 코드는 PyGObject로 간단한 윈도우를 하나 띄우는 예시 코드다.

```python
import gi
gi.require_version("Gtk", "3.0")
from gi.repository import Gtk

window = Gtk.Window(title="Hello World")
window.show()
window.connect("destroy", Gtk.main_quit)
Gtk.main()
```

`from gi.repository import Gtk`를 사용하고 있으나 gi.repository 를 찾아 들어가면 Gtk 모듈을 찾을 수 없다.
PyGObject의 gi/repository/\_\_init\_\_.py 파일을 보면 `sys.meta_path`[^meta-path]에 `DynamicImporter`를 추가[^add-dynamic-importer]하는 게 전부다.
`sys.meta_path`는 Python이 import 과정을 확장하는 방법 중의 하나다.
`DynamicImporter`[^dynamic-importer-class]는 Python Importer Protocol을 따르는 클래스로 `get_introspection_module` 함수를 호출해서 introspection으로 부터 모듈을 가져온다.

[^meta-path]: sys.meta_path는 Python이 import과정을 확장하는 방법 중 하나다. [python import](gi/repository/__init__.py#L25)
[^add-dynamic-importer]: [gi/importer.py#L103](https://gitlab.gnome.org/GNOME/pygobject/-/blob/1a2bc1d0806ab6178f65125bf0b2283eb3378d4d/gi/importer.py#103)
[^dynamic-importer-class]: [gi/repository/__init__.py#L25](https://gitlab.gnome.org/GNOME/pygobject/-/blob/1a2bc1d0806ab6178f65125bf0b2283eb3378d4d/gi/repository/__init__.py#L25)
https://gitlab.gnome.org/GNOME/pygobject/-/blob/1a2bc1d0806ab6178f65125bf0b2283eb3378d4d/gi/importer.py#L103

```python
# gi/repository/__init__.py 파일
from ..importer import DynamicImporter

# sys.meta_path에 DynamicImporter를 추가한다.
sys.meta_path.append(DynamicImporter('gi.repository'))
```

```python
class DynamicImporter(object):

    # Note: see PEP302 for the Importer Protocol implemented below.
    # ...

    def load_module(self, fullname):
        if fullname in sys.modules:
            return sys.modules[fullname]
        # ...
        with _check_require_version(namespace, stacklevel=stacklevel):
            try:
                # get_introspection_module 함수가 introspection에서 모듈을 생성한다.
                introspection_module = get_introspection_module(namespace)
            except RepositoryError as e:
                raise ImportError(e)
            # Import all dependencies first so their init functions
            # (gdk_init, ..) in overrides get called.
            # https://bugzilla.gnome.org/show_bug.cgi?id=656314
            for dep in repository.get_immediate_dependencies(namespace):
                importlib.import_module('gi.repository.' + dep.split("-")[0])
            dynamic_module = load_overrides(introspection_module)

        dynamic_module.__file__ = '<%s>' % fullname
        dynamic_module.__loader__ = self
        sys.modules[fullname] = dynamic_module

        return dynamic_module
```


IntrospectionModule[^introspection-module-class]은 `repository.find_by_name` 함수를 호출[^call-repository-find-by-name]하여 타입의 정보를 가져온 뒤
해당 정보를 바탕으로 Python 값을 만든다.
`repository.find_by_name`[^repository-find-by-name]은 g_irepository_find_by_name[^g-irepository-find-by-name]이라는 C로 작성된 GIRepository 라이브러리[^gi-repository-c]의 함수를 호출한다.

[^introspection-module-class]: [gi/module.py#L98](https://gitlab.gnome.org/GNOME/pygobject/-/blob/1a2bc1d0806ab6178f65125bf0b2283eb3378d4d/gi/module.py#L98)
[^call-repository-find-by-name]: [gi/module.py#L121](https://gitlab.gnome.org/GNOME/pygobject/-/blob/1a2bc1d0806ab6178f65125bf0b2283eb3378d4d/gi/module.py#L121)
[^repository-find-by-name]: [gi/pygi-repository.c#L130](https://gitlab.gnome.org/GNOME/pygobject/-/blob/1a2bc1d0806ab6178f65125bf0b2283eb3378d4d/gi/pygi-repository.c#L130)
[^g-irepository-find-by-name]: [g_irepository_find_by_name](https://developer.gnome.org/gi/stable/GIRepository.html#g-irepository-find-by-name)
[^gi-repository-c]: [GIRepository](https://developer.gnome.org/gi/stable/GIRepository.html)

```python
class IntrospectionModule(object):
    """An object which wraps an introspection typelib.

    This wrapping creates a python module like representation of the typelib
    using gi repository as a foundation. Accessing attributes of the module
    will dynamically pull them in and create wrappers for the members.
    These members are then cached on this introspection module.
    """

    # ...
    def __getattr__(self, name):
        # repository.find_by_name은 pygi-repository.c의_wrap_g_irepository_find_by_name를 부른다.
        info = repository.find_by_name(self._namespace, name)
        if not info:
            raise AttributeError("%r object has no attribute %r" % (
                                 self.__name__, name))

        # 이 뒤로는 info의 값에 맞게 Python Object를 만드는 과정이다.
        if isinstance(info, EnumInfo):
            g_type = info.get_g_type()
            wrapper = g_type.pytype

            if wrapper is None:
                # ...

                wrapper.__info__ = info
                wrapper.__module__ = 'gi.repository.' + info.get_namespace()
                # ...
                for value_info in info.get_values():
                    value_name = value_info.get_name_unescaped().translate(ascii_upper_trans)
                    setattr(wrapper, value_name, wrapper(value_info.get_value()))
                for method_info in info.get_methods():
                    setattr(wrapper, method_info.__name__, method_info)

            if g_type != TYPE_NONE:
                g_type.pytype = wrapper

        elif isinstance(info, RegisteredTypeInfo):
            # ...
        elif isinstance(info, FunctionInfo):
            wrapper = info
        elif isinstance(info, ConstantInfo):
            wrapper = info.get_value()
        else:
            raise NotImplementedError(info)

        # Cache the newly created wrapper which will then be
        # available directly on this introspection module instead of being
        # lazily constructed through the __getattr__ we are currently in.
        self.__dict__[name] = wrapper
        return wrapper

```


## GSettings 프로그램

GSettings[^25]는 어플리케이션이 설정파일을 저장할 수 있는 간단한 데이터베이스다.
도메인 이름을 역순으로 한것과 같은 application id를 지정해 어플리케이션벌 데이터를 나눈다.
`gsettings` 커맨드라인 도구로 저장된 데이터들 읽거나 수정할 수 있다.

gtweak/tweaks/tweak_group_xkb.py의 TypingTweakGroup[^26] class의 선언을 보면 GSettings의 schema로 org.gnome.desktop.input-sources[^27]를 쓰고 있고,
키로 xkb-options[^28]를 쓰고 있다. 따라서 Gnome Tweaks에서 옵션을 설정하면 GSettings의 디비를 업데이트한다는 것을 알 수 있다.

GSettings는 `gsettings`라는 CLI 프로그램으로 값을 읽거나 쓸 수 있다.
`gsettings get org.gnome.desktop.input-sources xkb-options`를 쉘에 입력하여 지금 설정된 값을 읽어올 수 있다.

```python
class TypingTweakGroup(Gtk.Box):

    XKB_GSETTINGS_SCHEMA = "org.gnome.desktop.input-sources"
    XKB_GSETTINGS_NAME = "xkb-options"

    # ...
```

[^25]: [Gsettings](https://developer.gnome.org/GSettings/)
[^26]: [gtweak/tweaks/tweak_group_xkb.py#L137](https://gitlab.gnome.org/GNOME/gnome-tweaks/-/blob/ebc0f25d361d172385302b9c9ba12503571a11cf/gtweak/tweaks/tweak_group_xkb.py#L137)
[^27]: [gtweak/tweaks/tweak_group_xkb.py#L139](https://gitlab.gnome.org/GNOME/gnome-tweaks/-/blob/ebc0f25d361d172385302b9c9ba12503571a11cf/gtweak/tweaks/tweak_group_xkb.py#L139)
[^28]: [gtweak/tweaks/tweak_group_xkb.py#L140](https://gitlab.gnome.org/GNOME/gnome-tweaks/-/blob/ebc0f25d361d172385302b9c9ba12503571a11cf/gtweak/tweaks/tweak_group_xkb.py#L140)

## 결론

Gnome Tweaks에서 옵션을 하나 바꾸었을 때 무슨 일이 일어나는지 알아보았다.
Gnome Tweaks는 Python으로 작성되었고, GTK를 사용한 UI 어플리케이션으로, UI의 이벤트를 받아
GSettings의 값을 수정한다.

코드를 읽는 과정에서 Python의 상속, GTK 어플리케이션의 구조, Python의 import 확장, GObject introspection에 대해 간단하게 살펴보았다.

