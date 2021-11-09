using System;
using System.IO;
using System.Threading.Tasks;
using Avalonia.Controls;
using Avalonia.Input;
using Avalonia.Interactivity;
using Avalonia.Markup.Xaml;
using Avalonia.Threading;
using LongArithm.Interpreter;

namespace ArithmGUI.Views
{
    public class MainWindow : Window
    {
        private readonly TextBox _codeBox;
        private readonly TextBox _consoleBox;
        private readonly MenuItem _runButton;
        private string _openedFilePath;
        private bool _ctrlPressed;

        public MainWindow()
        {
            InitializeComponent();
            Events.printed.Subscribe(PrintToConsole);
            Grid grid = this.FindControl<Grid>("Grid");
            _codeBox = this.Find<TextBox>( "CodeBox");
            _consoleBox = this.Find<TextBox>("ConsoleBox");
            _runButton = this.FindControl<MenuItem>("RunButton");
            _openedFilePath = "";
            grid.KeyDown += CtrlKeyboardEvent;
            grid.KeyUp += KeyboardEvent;
        }

        private void InitializeComponent()
        {
            AvaloniaXamlLoader.Load(this);
        }

        private void PrintToConsole(string msg)
        {
            Dispatcher.UIThread.Post(() =>
                _consoleBox.Text += $"\n{msg}");
        }
        
        private void CtrlKeyboardEvent(object? sender, KeyEventArgs e)
        {
            if (e.Key != Key.LeftCtrl) return;
            _ctrlPressed = true;
        }

        private void KeyboardEvent(object? sender, KeyEventArgs e)
        {
            if (!_ctrlPressed) return;
            switch (e.Key)
            {
                case Key.S:
                    Save();
                    break;
                case Key.O:
                    Open();
                    break;
                case Key.N:
                    New();
                    break;
            }
            _ctrlPressed = false;
        }
        
        private void OnFinish()
        {
            Dispatcher.UIThread.Post(() =>
            {
                _consoleBox.Text += "\nProcess finished";
                _runButton.IsEnabled = true;
            });
        }
        
        private void OnFinishWithErrors(string error)
        {
            Dispatcher.UIThread.Post(() =>
            {
                _consoleBox.Text += "\nProcess finished with errors";
                _consoleBox.Text += $"\n{error}";
                _runButton.IsEnabled = true;
            });
        }
        
        public void Start(object sender, RoutedEventArgs e)
        {
            _consoleBox.Text = "Processing...";
            _runButton.IsEnabled = false;
            var task = new Task(() =>
            {
                var res = Runners.runTryCatchErrors(_codeBox.Text);
                if (res.IsError)
                {
                    OnFinishWithErrors(res.ErrorValue);
                }
                else
                {
                    OnFinish();
                }
            });
            task.Start();
        }

        private async void SaveAs()
        {
            var dialog = new SaveFileDialog
            {
                InitialFileName = _openedFilePath
            };
            var path = await dialog.ShowAsync(this);
            if (path == null) return;
            await File.WriteAllTextAsync(path, _codeBox.Text);
            _openedFilePath = path;
        }

        private async void Save()
        {
            if (_openedFilePath == "") 
            {
                SaveAs();
            }
            else
            {
                await File.WriteAllTextAsync(_openedFilePath, _codeBox.Text);
            }
        }

        private async void Open()
        {
            Save();
            var dialog = new OpenFileDialog();
            dialog.Filters.Add(new FileDialogFilter { Extensions = { "txt" } });
            var path = await dialog.ShowAsync(this);
            if (path is not {Length: > 0}) return;
            _codeBox.Text = await File.ReadAllTextAsync(path[0]);
            _openedFilePath = path[0];
        }

        private void New()
        {
            Save();
            _codeBox.Text = "";
            _openedFilePath = "";
            Save();
        }
        
        public void OpenEvent(object sender, RoutedEventArgs e)
        {
            Open();
        }

        private void SaveEvent(object sender, RoutedEventArgs e)
        {
            Save();
        }
        
        private void SaveAsEvent(object sender, RoutedEventArgs e)
        {
            SaveAs();
        }
        
        public void NewEvent(object sender, RoutedEventArgs e)
        {
            New();
        }

    }
}
