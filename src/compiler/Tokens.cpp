//
// Created by jeuxj on 29/10/2022.
//

#include "Tokens.h"
#include <optional>
#include <unordered_map>
#include <cwctype>
#include <cassert>

using namespace Paduset;

const std::vector<Token>& Tokenizer::Read() {
    Token recognizedToken{};

    while (NextChar()) {
        if (!inStringLiteral) {
            if (RecognizeStringLiteralStart()) {
                inStringLiteral = true;
                continue;
            }

            if (IsValidIdentifierCharacter(character)) {
                identifierCharCount += characterBytes;
            } else {
                // "Flush" the identifier if necessary.
                FlushIdentifier();
            }

            if (SkipChar()) {
                continue;
            }

            UpdateWordCharSpan();

            utf8::append((char32_t) character, word);
            utf8::append((char32_t) std::towlower(wchar_t(character)), wordLowerCase);

            if (RecognizeNumber(recognizedToken) ||
                RecognizeKeyword(recognizedToken) ||
                RecognizeOperator(recognizedToken) ||
                RecognizeSeparator(recognizedToken)) {
                AddToken(recognizedToken);
                recognizedToken = EmptyToken; // Empty
            }
        } else {
            if (inEscapeSequence) {
                UpdateWordCharSpan();
                utf8::append((char32_t) character, word); // newlines too?
                inEscapeSequence = false;
            } else if (RecognizeEscapeSequenceStart()) {
                inEscapeSequence = true;
            } else if (RecognizeStringLiteralEnd(recognizedToken)) {
                AddToken(recognizedToken);
                recognizedToken = EmptyToken; // Empty
                inStringLiteral = false;
            } else {
                UpdateWordCharSpan();
                utf8::append((char32_t) character, word);
            }
        }
    }

    // Add the last identifier just in case
    FlushIdentifier();

    return tokens;
}

void Tokenizer::UpdateWordCharSpan() {
    // Minus one because we're now *past* that character.
    if (word.empty()) {
        wordStartCharIndex = size_t(stream.tellg()) - size_t(1);
    }
    wordEndCharIndex = size_t(stream.tellg()) - size_t(1);
}

void Tokenizer::FlushIdentifier() {
    if (identifierCharCount > 0) {
        std::string garbage = word;
        std::string identifier = word;

        size_t startCharsRemoved = word.length() - identifierCharCount;
        garbage.erase(startCharsRemoved);
        identifier.erase(0, startCharsRemoved);

        if (garbage.length() != 0) {
            AddToken(Token::Make<TokenKind::I_Garbage>(garbage, GetWordSpan(0, word.length() - startCharsRemoved)));
        }
        if (identifier.length() != 0) {
            consecutiveLowerCaseWords.push_back(wordLowerCase);
            AddToken(Token::Make<TokenKind::I_Identifier>(identifier, GetWordSpan(startCharsRemoved)));
        }
    }
}

bool Tokenizer::NextChar() {
    if (pendingNextCharacter) {
        character = pendingNextCharacter;
        characterBytes = pendingNextCharacterBytes;
        pendingNextCharacter = 0;
        pendingNextCharacterBytes = 0;
        return true;
    }
    if (streamIter != streamIterEnd) {
        character = utf8::next(streamIter, streamIterEnd);
        characterBytes = UtfCharacterBytes(character);
        return true;
    }
    return false;
}

bool Tokenizer::SkipChar() const {
    if (std::iswspace(static_cast<int32_t>(character))) {
        return true;
    } else {
        return false;
    }
}

bool Tokenizer::RecognizeNumber(Token& token) {
    bool hasDecimalSeparator = false;
    bool fullyFormed = false;

    // Validate the number
    for (const char numChar: word) {
        if (std::isdigit(numChar)) {
            fullyFormed = true;
        } else if (!hasDecimalSeparator && numChar == '.') {
            hasDecimalSeparator = true;
            fullyFormed = false;
        } else {
            return false;
        }
    }

    // Only a dot lol
    if (!fullyFormed) {
        return false;
    }

    // Incoming decimal separator or number...
    uint32_t peeked = PeekChar();
    if (peeked == '.' || std::iswdigit(peeked)) {
        return false;
    }

    // TODO: React to excessively large numbers
    if (hasDecimalSeparator) {
        float value = std::stof(word);
        token = Token::Make<TokenKind::L_Real>(value, GetWordSpan());
    } else {
        int value = std::stoi(word);
        token = Token::Make<TokenKind::L_Integer>(value, GetWordSpan());
    }

    return true;
}

bool Tokenizer::RecognizeKeyword(Token& token) {
    static const std::unordered_map<std::string, TokenKind> matchMap{
            {"programme", TokenKind::KW_Program},
            {"variables", TokenKind::KW_Variables},
            {"début",     TokenKind::KW_Begin},
            {"debut",     TokenKind::KW_Begin},
            {"fin",       TokenKind::KW_End},
            {"réel",      TokenKind::KW_Real},
            {"reel",      TokenKind::KW_Real},
            {"entier",    TokenKind::KW_Integer},
            {"booléen",   TokenKind::KW_Boolean},
            {"booleen",   TokenKind::KW_Boolean},
            {"chaîne",    TokenKind::KW_String},
            {"chaine",    TokenKind::KW_String},
            {"écrire",    TokenKind::KW_Write},
            {"ecrire",    TokenKind::KW_Write},
            {"lire",      TokenKind::KW_Read},
            {"si",        TokenKind::KW_If},
            {"sinon",     TokenKind::KW_Else},
            {"alors",     TokenKind::KW_Then}
    };

    // Don't do anything if the next character indicates that an identifier
    // or another keyword may continue.
    // Only do recognition once we have the full word.
    // Example: silouhette
    //          ...won't get interpreted as "si" and "louhette", but as "silouhette"
    //          si(a<b)
    //          ...will get interpreted as "si", '(', etc.
    if (IsValidIdentifierCharacter(PeekChar())) {
        return false;
    }

    auto result = matchMap.find(wordLowerCase);
    if (result != matchMap.end()) {
        token = Token::MakeNoContent(result->second, GetWordSpan());
        return true;
    } else {
        return false;
    }
}

bool Tokenizer::RecognizeOperator(Token& token) {
    static const std::unordered_map<std::string, TokenKind> matchMap{
            {">",   TokenKind::OP_GreaterThan},
            {"<-",  TokenKind::OP_Assign},
            {"<",   TokenKind::OP_LowerThan},
            {"=",   TokenKind::OP_Equals},
            {"+",   TokenKind::OP_Add},
            {"-",   TokenKind::OP_Subtract},
            {"*",   TokenKind::OP_Multiply},
            {"/",   TokenKind::OP_Divide},
            {"mod", TokenKind::OP_Modulo},
            {"div", TokenKind::OP_IntegerDivide},
            {":",   TokenKind::OP_Declare},
    };

    auto result = matchMap.find(wordLowerCase);
    if (result != matchMap.end()) {
        // Edge cases for operators beginning with the same characters.
        if (character == '<' && PeekChar() == '-') {
            return false;
        }
        // Avoid mistaking identifiers as operators.
        if (result->second == TokenKind::OP_Modulo || result->second == TokenKind::OP_IntegerDivide) {
            if (IsValidIdentifierCharacter(PeekChar())) {
                return false;
            }
        }
        token = Token::MakeNoContent(result->second, GetWordSpan());
        return true;
    } else {
        return false;
    }
}

bool Tokenizer::RecognizeStringLiteralStart() const {
    return character == '"' || character == U'«' || character == U'“';
}

bool Tokenizer::RecognizeStringLiteralEnd(Token& token) {
    if (character == '"' || character == U'»' || character == U'”') {
        token = Token::Make<TokenKind::L_String>(word, GetWordSpan(-1, 1));
        return true;
    } else {
        return false;
    }
}

bool Tokenizer::RecognizeEscapeSequenceStart() const {
    return character == '\\';
}

void Tokenizer::AddToken(const Token& token) {
    word = "";
    wordLowerCase = "";
    identifierCharCount = 0;
    tokens.push_back(token);

    if (token.kind != TokenKind::I_Identifier) {
        consecutiveLowerCaseWords.clear();
    } else {
        MergeCompositeTokens();
    }
}

struct CompositeContext {
    std::vector<Token>* tokens;
    size_t wordCount;
    std::string* wordPrev1;
    std::string* wordPrev2;
    std::string* wordPrev3;
    std::string* wordPrev4;
    std::string* wordPrev5;
};

// EXPERIMENTAL!
template<typename... P5, typename... P4, typename... P3, typename... P2, typename... P1>
bool MergeLastTokens(const CompositeContext& context, TokenKind kind,
                     std::tuple<P5...> previous5, std::tuple<P4...> previous4, std::tuple<P3...> previous3,
                     std::tuple<P2...> previous2, std::tuple<P1...> previous1) {
    constexpr int totalSize = sizeof...(P5) > 0 ? 5 :
            sizeof...(P4) > 0 ? 4 :
            sizeof...(P3) > 0 ? 3 :
            sizeof...(P2) > 0 ? 2 :
            sizeof...(P1) > 0 ? 1 : 0;

    if (context.wordCount < totalSize) {
        return false;
    }

    bool onTrack = true;
    std::apply([&](auto&& ... args) {
        onTrack = (onTrack && ... && (args == *context.wordPrev5));
    }, previous5);
    if (!onTrack) {
        return false;
    }
    std::apply([&](auto&& ... args) {
        onTrack = (onTrack && ... && (args == *context.wordPrev4));
    }, previous4);
    if (!onTrack) {
        return false;
    }
    std::apply([&](auto&& ... args) {
        onTrack = (onTrack && ... && (args == *context.wordPrev3));
    }, previous3);
    if (!onTrack) {
        return false;
    }
    std::apply([&](auto&& ... args) {
        onTrack = (onTrack && ... && (args == *context.wordPrev3));
    }, previous2);
    if (!onTrack) {
        return false;
    }
    std::apply([&](auto&& ... args) {
        onTrack = (onTrack && ... && (args == *context.wordPrev3));
    }, previous1);
    if (!onTrack) {
        return false;
    }

    // Do it!

    size_t tokensVecSize = context.tokens->size();

    Token& closestToken = (*context.tokens)[tokensVecSize - 1];
    Token& furthestToken = (*context.tokens)[tokensVecSize - totalSize];

    CharSpan span = closestToken.span.CombineWith(furthestToken.span);

    Token token = Token::MakeNoContent(kind, span);
    for (int i = 0; i < totalSize; ++i) {
        context.tokens->pop_back();
    }

    context.tokens->push_back(token);
    return true;
}

void Tokenizer::MergeCompositeTokens() {
    // All composite tokens (sorted by size):
    //
    // 5-length:
    // - EST SUPÉRIEUR OU ÉGAL À     : >=
    // - EST INFÉRIEUR OU ÉGAL À     : <=
    //
    // 4-length:
    // - EST STRICTEMENT SUPÉRIEUR À : <
    // - EST STRICTEMENT INFÉRIEUR À : >
    //
    // 3-length:
    // - EST ÉGAL À                  : >=
    // - EST DIFFÉRENT DE            : ≠

    const auto findSpan = [this](int consumedTokens) -> CharSpan {
        assert(tokens.size() >= size_t(consumedTokens));

        Token& closestToken = tokens[tokens.size() - 1];
        Token& furthestToken = tokens[tokens.size() - consumedTokens];

        return closestToken.span.CombineWith(furthestToken.span);
    };

    const auto mergeLastTokens = [this](const Token& token, int consumedTokens) {
        for (int i = 0; i < consumedTokens; ++i) {
            tokens.pop_back();
        }

        tokens.push_back(token);
    };

    size_t wordCount = consecutiveLowerCaseWords.size();
    if (wordCount < 3) {
        return;
    }

    std::string* wordPrev1 = &consecutiveLowerCaseWords[wordCount - 1];
    std::string* wordPrev2 = &consecutiveLowerCaseWords[wordCount - 2];
    std::string* wordPrev3 = &consecutiveLowerCaseWords[wordCount - 3];
    std::string* wordPrev4 = wordCount >= 4 ? &consecutiveLowerCaseWords[wordCount - 4] : nullptr;
    std::string* wordPrev5 = wordCount >= 5 ? &consecutiveLowerCaseWords[wordCount - 5] : nullptr;

    // CompositeContext ctx{&tokens, wordCount, wordPrev1, wordPrev2, wordPrev3, wordPrev4, wordPrev5};

    // All of this could be heavily optimized, with less redundancy and all,
    // but this form is easily maintainable and readable.

     if (wordCount >= 3)
    {
        if (*wordPrev3 == "est") {
            // EST ÉGAL À
            if (*wordPrev2 == "égal" || *wordPrev2 == "egal") {
                if (*wordPrev1 == "à" || *wordPrev1 == "a") {
                    mergeLastTokens(Token::Make<TokenKind::OP_Equals>(findSpan(3)), 3);
                    return;
                }
            }

            // EST DIFFÉRENT DE
            if (*wordPrev2 == "différent" || *wordPrev2 == "different") {
                if (*wordPrev1 == "de") {
                    mergeLastTokens(Token::Make<TokenKind::OP_NotEquals>(findSpan(3)), 3);
                    return;
                }
            }
        }
    }
    if (wordCount >= 4) {
        if (*wordPrev4 == "est") {
            if (*wordPrev3 == "strictement") {
                // EST STRICTEMENT SUPÉRIEUR À
                if (*wordPrev2 == "supérieur" || *wordPrev2 == "superieur") {
                    if (*wordPrev1 == "à" || *wordPrev1 == "a") {
                        mergeLastTokens(Token::Make<TokenKind::OP_GreaterThan>(findSpan(4)), 4);
                        return;
                    }
                }

                // EST STRICTEMENT INFÉRIEUR À
                if (*wordPrev2 == "inférieur" || *wordPrev2 == "inferieur") {
                    if (*wordPrev1 == "à" || *wordPrev1 == "a") {
                        mergeLastTokens(Token::Make<TokenKind::OP_LowerThan>(findSpan(4)), 4);
                        return;
                    }
                }
            }
        }
    }
    if (wordCount >= 5) {
        if (*wordPrev5 == "est") {
            // EST SUPÉRIEUR OU ÉGAL À
            if (*wordPrev4 == "supérieur" || *wordPrev4 == "superieur") {
                if (*wordPrev3 == "ou") {
                    if (*wordPrev2 == "égal" || *wordPrev2 == "egal") {
                        if (*wordPrev1 == "à" || *wordPrev1 == "a") {
                            mergeLastTokens(Token::Make<TokenKind::OP_GreaterOrEqualThan>(findSpan(5)), 5);
                            return;
                        }
                    }
                }
            }

            // EST INFÉRIEUR OU ÉGAL À
            if (*wordPrev4 == "inférieur" || *wordPrev4 == "inferieur") {
                if (*wordPrev3 == "ou") {
                    if (*wordPrev2 == "égal" || *wordPrev2 == "egal") {
                        if (*wordPrev1 == "à" || *wordPrev1 == "a") {
                            mergeLastTokens(Token::Make<TokenKind::OP_LowerOrEqualThan>(findSpan(5)), 5);
                            return;
                        }
                    }
                }
            }
        }
    }
}

void Tokenizer::PrintDebugInformation(std::ostream& debugStream) {
    debugStream << "Tokens read: \n";
    for (const auto& token: tokens) {
        token.PrintDebugInformation(debugStream);
        debugStream << "\n";
    }
    std::flush(std::cout);
}

bool Tokenizer::IsValidIdentifierCharacter(uint32_t ch) {
    // Avoid an identifier starting by a number.
    bool isFirstChar = identifierCharCount <= 0;
    if ((isFirstChar && std::isdigit(int(ch))) || (!word.empty() && isdigit(word[0]))) {
        return false;
    }

    // Letters or numbers only. (even emojis soon??)
    // Also, this breaks for 24-bit or 32-bit characters obviously,
    // but fixing this will require ICU which is annoying.
    return std::iswalnum(wint_t(ch));
}

uint32_t Tokenizer::PeekChar() {
    if (streamIter == streamIterEnd) {
        return 0;
    }
    // For some BIZARRE reason, utf8::peek_next does not peek at all. So there's this.
    if (pendingNextCharacter == 0) {
        pendingNextCharacter = utf8::next(streamIter, streamIterEnd);
        pendingNextCharacterBytes = UtfCharacterBytes(pendingNextCharacter);
    }
    return pendingNextCharacter;
}

bool Tokenizer::RecognizeSeparator(Token& token) {
    if (character == '(') {
        token = Token::Make<TokenKind::S_LeftParens>(GetWordSpan());
    } else if (character == ')') {
        token = Token::Make<TokenKind::S_RightParens>(GetWordSpan());
    } else if (character == ',') {
        token = Token::Make<TokenKind::S_Comma>(GetWordSpan());
    } else {
        return false;
    }

    return true;
}

CharSpan Tokenizer::GetWordSpan(size_t offsetStart, size_t offsetEnd) {
    return {wordStartCharIndex + offsetStart, wordEndCharIndex - offsetEnd};
}

int Tokenizer::UtfCharacterBytes(uint32_t ch) {
    if (ch <= 0x00007F) {
        return 1;
    } else if (ch <= 0x0007FF) {
        return 2;
    } else if (ch <= 0x00FFFF) {
        return 3;
    } else {
        return 4;
    }
}

void Token::PrintDebugInformation(std::ostream& stream) const {
    stream << "[" << TokenName(kind);
    if (RequiresIntContent(kind)) {
        stream << ", int: " << content.intValue;
    } else if (RequiresStringContent(kind)) {
        stream << ", str: \"" << *stringContent << "\"";
    } else if (RequiresFloatContent(kind)) {
        stream << ", float: " << content.floatValue;
    } else if (RequiresBoolContent(kind)) {
        stream << ", bool: " << content.boolValue;
    }
    stream << " | (" << span.startChar << "," << span.endChar << ")";
    stream << "]";
}

CharSpan::CharSpan(size_t startChar, size_t endChar) : startChar(startChar), endChar(endChar) {}

CharSpan CharSpan::CombineWith(const CharSpan& other) {
    size_t newStart = this->startChar > other.startChar ? other.startChar : this->startChar;
    size_t newEnd = other.endChar > this->endChar ? other.endChar : this->endChar;

    return {newStart, newEnd};
}

TokenSpan TokenSpan::CombineWith(const TokenSpan& other) const {
    const Token* newStart;
    const Token* newEnd;

    // We start too late? Then, the other takes over.
    if (this->start.span.startChar > other.start.span.startChar) {
        newStart = &other.start;
    } else {
        newStart = &this->start;
    }

    // We go farther away? Then, we take over.
    if (this->end.span.startChar > other.end.span.startChar) {
        newEnd = &this->end;
    } else {
        newEnd = &other.end;
    }

    return TokenSpan{*newStart, *newEnd};
}

CharSpan TokenSpan::ToCharSpan() const {
    return {start.span.startChar, end.span.endChar};
}

CharSpan TokenSpan::BeginningCursor() const {
    size_t cursor = start.span.startChar;
    return CharSpan(cursor, cursor);
}

CharSpan TokenSpan::EndingCursor() const {
    size_t cursor = end.span.endChar;
    return CharSpan(cursor, cursor);
}
