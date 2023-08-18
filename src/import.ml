module Text_document = Lsp.Text_document
module Uri = Lsp.Uri
module Json = Lsp.Import.Json
open! Lsp

include struct
  open Lsp.Types
  module ClientCapabilities = ClientCapabilities
  module CodeAction = CodeAction
  module CodeActionKind = CodeActionKind
  module CodeActionOptions = CodeActionOptions
  module CodeActionParams = CodeActionParams
  module CodeActionResult = CodeActionResult
  module CodeActionRegistrationOptions = CodeActionRegistrationOptions
  module CodeLens = CodeLens
  module CodeLensOptions = CodeLensOptions
  module CodeLensParams = CodeLensParams
  module Command = Command
  module CompletionItem = CompletionItem
  module CompletionItemKind = CompletionItemKind
  module CompletionList = CompletionList
  module CompletionOptions = CompletionOptions
  module CompletionParams = CompletionParams
  module ConfigurationParams = ConfigurationParams
  module CreateFile = CreateFile
  module Diagnostic = Diagnostic
  module DiagnosticRelatedInformation = DiagnosticRelatedInformation
  module DiagnosticSeverity = DiagnosticSeverity
  module DiagnosticTag = DiagnosticTag
  module DidChangeConfigurationParams = DidChangeConfigurationParams
  module DidChangeWorkspaceFoldersParams = DidChangeWorkspaceFoldersParams
  module DidOpenTextDocumentParams = DidOpenTextDocumentParams
  module Diff = Lsp.Diff
  module DocumentFilter = DocumentFilter
  module DocumentHighlight = DocumentHighlight
  module DocumentHighlightKind = DocumentHighlightKind
  module DocumentHighlightParams = DocumentHighlightParams
  module DocumentSymbol = DocumentSymbol
  module DocumentUri = DocumentUri
  module ExecuteCommandOptions = ExecuteCommandOptions
  module ExecuteCommandParams = ExecuteCommandParams
  module FoldingRange = FoldingRange
  module FoldingRangeParams = FoldingRangeParams
  module Hover = Hover
  module HoverParams = HoverParams
  module InitializeParams = InitializeParams
  module InitializeResult = InitializeResult
  module Location = Location
  module LogMessageParams = LogMessageParams
  module MarkupContent = MarkupContent
  module MarkupKind = MarkupKind
  module MessageType = MessageType

  module OptionalVersionedTextDocumentIdentifier =
    OptionalVersionedTextDocumentIdentifier

  module ParameterInformation = ParameterInformation
  module PositionEncodingKind = PositionEncodingKind
  module ProgressParams = ProgressParams
  module ProgressToken = ProgressToken
  module PublishDiagnosticsParams = PublishDiagnosticsParams

  module PublishDiagnosticsClientCapabilities =
    PublishDiagnosticsClientCapabilities

  module ReferenceParams = ReferenceParams
  module Registration = Registration
  module RegistrationParams = RegistrationParams
  module RenameOptions = RenameOptions
  module RenameParams = RenameParams
  module SaveOptions = SaveOptions
  module SelectionRange = SelectionRange
  module SelectionRangeParams = SelectionRangeParams
  module SemanticTokens = SemanticTokens
  module SemanticTokensEdit = SemanticTokensEdit
  module SemanticTokensLegend = SemanticTokensLegend
  module SemanticTokensDelta = SemanticTokensDelta
  module SemanticTokensDeltaParams = SemanticTokensDeltaParams
  module SemanticTokenModifiers = SemanticTokenModifiers
  module SemanticTokensOptions = SemanticTokensOptions
  module SemanticTokensParams = SemanticTokensParams
  module SemanticTokenTypes = SemanticTokenTypes
  module ServerCapabilities = ServerCapabilities
  module Server_notification = Lsp.Server_notification
  module SetTraceParams = SetTraceParams
  module ShowDocumentClientCapabilities = ShowDocumentClientCapabilities
  module ShowDocumentParams = ShowDocumentParams
  module ShowDocumentResult = ShowDocumentResult
  module ShowMessageParams = ShowMessageParams
  module SignatureHelp = SignatureHelp
  module SignatureHelpOptions = SignatureHelpOptions
  module SignatureHelpParams = SignatureHelpParams
  module SignatureInformation = SignatureInformation
  module SymbolInformation = SymbolInformation
  module SymbolKind = SymbolKind
  module TextDocumentClientCapabilities = TextDocumentClientCapabilities
  module TextDocumentContentChangeEvent = TextDocumentContentChangeEvent
  module TextDocumentEdit = TextDocumentEdit
  module TextDocumentFilter = TextDocumentFilter
  module TextDocumentIdentifier = TextDocumentIdentifier
  module TextDocumentItem = TextDocumentItem
  module TextDocumentRegistrationOptions = TextDocumentRegistrationOptions
  module TextDocumentSyncKind = TextDocumentSyncKind
  module TextDocumentSyncOptions = TextDocumentSyncOptions
  module TextDocumentSyncClientCapabilities = TextDocumentSyncClientCapabilities
  module TextEdit = TextEdit

  module TraceValue = TraceValues
  (** deprecated *)

  module TraceValues = TraceValues
  module Unregistration = Unregistration
  module UnregistrationParams = UnregistrationParams
  module VersionedTextDocumentIdentifier = VersionedTextDocumentIdentifier
  module WorkDoneProgressBegin = WorkDoneProgressBegin
  module WorkDoneProgressCreateParams = WorkDoneProgressCreateParams
  module WorkDoneProgressEnd = WorkDoneProgressEnd
  module WorkDoneProgressReport = WorkDoneProgressReport
  module WorkspaceEdit = WorkspaceEdit
  module WorkspaceFolder = WorkspaceFolder
  module WorkspaceFoldersChangeEvent = WorkspaceFoldersChangeEvent
  module WorkspaceSymbolParams = WorkspaceSymbolParams
  module WorkspaceFoldersServerCapabilities = WorkspaceFoldersServerCapabilities
end
